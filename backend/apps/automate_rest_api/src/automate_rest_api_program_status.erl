%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_status).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_status_update/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").

-record(state, { program_id :: binary() }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #state{ program_id=ProgramId }}.

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
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X, {edit_program_status, ProgramId}) of
                        {true, UserId} ->
                            case automate_storage:is_user_allowed({user, UserId}, ProgramId, edit_program) of
                                {ok, true} ->
                                    { true, Req1, State };
                                {ok, false} ->
                                    { { false, <<"Unauthorized">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_status_update}],
     Req, State}.

-spec accept_status_update(_, #state{}) -> {boolean(),_,#state{}}.
accept_status_update(Req, State=#state{program_id=ProgramId}) ->
    {ok, Body, _} = ?UTILS:read_body(Req),
    #{<<"enable">> := Status } = jiffy:decode(Body, [return_maps]),

    case automate_bot_engine:change_program_status(ProgramId, Status) of
        ok ->

            Output = jiffy:encode(#{ <<"success">> => true
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { true, Res3, State };
        {error, _} ->
            Output = jiffy:encode(#{ <<"success">> => false
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { false, Res3, State }
    end.
