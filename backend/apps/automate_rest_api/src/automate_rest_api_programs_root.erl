%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_programs_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).
-export([accept_json_create_program/2]).
-export([resource_exists/2]).

-include("./records.hrl").

-record(create_program_seq, { username }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #create_program_seq{ username=UserId }}.

resource_exists(Req, State) ->
    {false, Req, State}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    #create_program_seq{username=Username} = State,
                    case automate_rest_api_backend:is_valid_token(X) of
                        {true, Username} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching username
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    io:fwrite("Control types accepted~n", []),
    {[{{<<"application">>, <<"json">>, []}, accept_json_create_program}],
     Req, State}.

-spec accept_json_create_program(cowboy_req:req(), #create_program_seq{})
                                   -> {'true',cowboy_req:req(),_}.
accept_json_create_program(Req, State) ->
    #create_program_seq{username=Username} = State,
    case automate_rest_api_backend:create_program(Username) of
        { ok, {ProgramId, ProgramName, ProgramUrl} } ->

            Output = jiffy:encode(#{ <<"id">> => ProgramId
                                   , <<"name">> => ProgramName
                                   , <<"link">> =>  ProgramUrl
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { {true, ProgramUrl }, Res3, State }
    end.

