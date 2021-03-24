%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_variables_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , resource_exists/2
        , content_types_accepted/2
        ]).

-export([ to_json/2
        , accept_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { program_id :: binary() }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #state{ program_id=ProgramId
            }}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            { false, Req, State };
        _ ->
            { true, Req, State}
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PATCH">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        Method ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    Scope = case Method of
                                <<"GET">> ->  {read_program_variables, ProgramId};
                                <<"PATCH">> -> {edit_program_variables, ProgramId}
                            end,
                    case automate_rest_api_backend:is_valid_token_uid(X, Scope) of
                        {true, UserId} ->
                            case automate_storage:is_user_allowed({user, UserId}, ProgramId, edit_program) of
                                {ok, true} ->
                                    { true, Req1, State };
                                {ok, false} ->
                                    { { false, <<"Not authorized">> }, Req1, State}
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{ program_id=ProgramId} = State,
    case automate_storage:get_program_variables(ProgramId) of
        { ok, VariableMap } ->
            Output = jiffy:encode(#{ variables => ?FORMATTING:serialize_variable_map(VariableMap)}),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


%% PATCH handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json}],
     Req, State}.

accept_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> ->
            update_variables(Req, State)
    end.


update_variables(Req, State=#state{program_id=ProgramId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    #{ <<"values">> := Values } = jiffy:decode(Body, [return_maps]),
    %% Wrap all in the same transaction
    {atomic, ok} = mnesia:transaction(fun() ->
                                              ok = lists:foreach(fun(#{ <<"name">> := Name, <<"value">> := Value}) ->
                                                                         ok = automate_bot_engine_variables:set_program_variable(ProgramId, Name, Value)
                                                                 end, Values)
                                      end),
    Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true }), Req1),
    { true, Req2, State }.
