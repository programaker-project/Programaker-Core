%%% @doc
%%% REST endpoint to manipulate program checkpoints.
%%% @end

-module(automate_rest_api_program_specific_checkpoint).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).

-export([ accept_json_program/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(get_program_seq, { user_id, program_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramId = cowboy_req:binding(program_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #get_program_seq{ user_id=UserId
                      , program_id=ProgramId
                      }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("[SPProgram]Asking for methods~n", []),
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    #get_program_seq{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching username
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_program}],
     Req, State}.

accept_json_program(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            checkpoint_program(Req, State)
    end.

%% POST handler
checkpoint_program(Req, State=#get_program_seq{program_id=ProgramId, user_id=UserId}) ->

    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    case automate_storage:checkpoint_program(UserId, ProgramId, Parsed) of
        ok ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

send_json_output(Output, Req) ->
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2).
