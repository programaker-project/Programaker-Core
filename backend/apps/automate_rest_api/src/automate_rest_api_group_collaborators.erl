%%% @doc
%%% REST endpoint to manage group collaborators.
%%% @end

-module(automate_rest_api_group_collaborators).
-export([init/2]).
-export([ allowed_methods/2
        , is_authorized/2
        , content_types_provided/2
        , options/2
        ]).

-export([ to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-define(PROGRAMS, automate_rest_api_utils_programs).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { user_id :: binary() | undefined, group_id :: binary()}).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    GroupId = cowboy_req:binding(group_id, Req),
    {cowboy_rest, Req, #state{ user_id=undefined, group_id=GroupId }}.

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
is_authorized(Req, State=#state{group_id=GroupId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        Method ->
            Check = case Method of
                        <<"GET">> -> fun automate_storage:is_allowed_to_read_in_group/2;
                        _ -> fun automate_storage:is_allowed_to_write_in_group/2
                    end,
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            case Check({user, UserId}, GroupId) of
                                true -> { true, Req1, State#state{ user_id=UserId } };
                                false ->
                                    { { false, <<"Operation not allowed">>}, Req1, State }
                            end;
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
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{user_id=_UserId, group_id=GroupId}) ->
    case automate_storage:list_collaborators({group, GroupId}) of
        { ok, Collaborators } ->
            Output = jiffy:encode(#{ success => true, collaborators => lists:map(fun ?FORMATTING:collaborator_to_json/1 , Collaborators)}),
            Res = ?UTILS:send_json_format(Req),

            { Output, Res, State }
    end.
