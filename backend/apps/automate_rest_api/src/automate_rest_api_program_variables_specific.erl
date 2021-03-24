%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_variables_specific).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , resource_exists/2
        , delete_resource/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { program_id :: binary()
               , var_name :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    VarName = cowboy_req:binding(var_name, Req),

    {cowboy_rest, Req
    , #state{ program_id=ProgramId
            , var_name=VarName
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
    {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId}) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X, { edit_program_variables, ProgramId }) of
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


%% DELETE handler
delete_resource(Req, State=#state{program_id=ProgramId, var_name=VarName}) ->
    case automate_bot_engine_variables:delete_program_variable(ProgramId, VarName) of
        ok ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req),
            { false, Req1, State }
    end.
