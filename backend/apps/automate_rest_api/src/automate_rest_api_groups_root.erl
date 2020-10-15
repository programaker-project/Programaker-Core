%%% @doc
%%% REST endpoint to manage groups.
%%% @end

-module(automate_rest_api_groups_root).
-export([init/2]).
-export([ allowed_methods/2
        , is_authorized/2
        , content_types_accepted/2
        , options/2
        ]).

-export([ accept_json/2
        ]).
-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { user_id :: binary() | undefined}).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req, #state{ user_id=undefined }}.

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
is_authorized(Req, State) ->
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
                        {true, UserId} ->
                            { true, Req1, State#state{ user_id=UserId } };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.


content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json}],
     Req, State}.

%% POST
accept_json(Req, State=#state{ user_id=UserId }) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    Collaborators = case Parsed of
                        #{ <<"collaborators">> := Collabs } -> Collabs;
                        _ -> []
                    end,
    case automate_storage:create_group(maps:get(<<"name">>, Parsed), UserId, maps:get(<<"public">>, Parsed)) of
        {ok, Group=#user_group_entry{ id=GroupId }} ->
            ok = automate_storage:add_collaborators({ group, GroupId }, lists:map(fun(#{ <<"id">> := UId, <<"role">> := RoleStr }) ->
                                                                                          Role = case RoleStr of
                                                                                                     <<"admin">> -> admin;
                                                                                                     <<"editor">> -> editor;
                                                                                                     <<"viewer">> -> viewer
                                                                                                 end,
                                                                                          {UId, Role}
                                                                                  end,
                                                                                  Collaborators)),
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ success => true
                                                         , group => ?FORMATTING:group_to_json(Group)
                                                         }), Req),
            { true, Req2, State };
        {error, already_exists} ->

            Res = cowboy_req:reply(409, %% Conflict
                                   #{ <<"content-type">> => <<"application/json">> },
                                   jiffy:encode(#{ <<"success">> => false
                                                 , <<"error">> => already_exists
                                                 }),
                                   Req1),
            { stop, Res, State };
        {error, Reason} ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false
                                                         , <<"error">> => unknown
                                                         , <<"debug">> => list_to_binary(lists:flatten(io_lib:format("~p", [Reason])))
                                                         })
                                          , Req1),
            { false, Req2, State }
    end.
