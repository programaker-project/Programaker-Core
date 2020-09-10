%%% @doc
%%% REST endpoint to manage groups.
%%% @end

-module(automate_rest_api_group_specific).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        , delete_resource/2
        ]).

-export([ accept_changes/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { group_id :: binary() }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    GroupId = cowboy_req:binding(group_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ group_id=GroupId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{group_id=GroupId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        _ ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UId} ->
                            case automate_storage:is_allowed_to_admin_in_group({user, UId}, GroupId) of
                                true ->
                                    { true, Req1, State };
                                false ->
                                    { { false, <<"Action not authorized">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% Modifiers
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_changes}],
     Req, State}.

accept_changes(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> ->
            update_group_metadata(Req, State)
    end.

%% PATCH handler
update_group_metadata(Req, State=#state{group_id=GroupId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    case automate_storage:update_group_metadata(GroupId, body_to_metadata_edition(Parsed)) of
        ok ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% DELETE handler
delete_resource(Req, State=#state{group_id=GroupId}) ->
    case ?UTILS:group_has_picture(GroupId) of
        true ->
            ok = file:delete(?UTILS:group_picture_path(GroupId));
        _ -> ok
    end,
    case automate_storage:delete_group(GroupId) of
        ok ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req),
            { false, Req1, State }
    end.


body_to_metadata_edition(Parsed) ->
    AsList = lists:filtermap(fun({K, V}) ->
                                     case K of
                                         <<"public">> -> {true, {public, V} };
                                         _ -> false
                                     end
                             end, maps:to_list(Parsed)),
    maps:from_list(AsList).
