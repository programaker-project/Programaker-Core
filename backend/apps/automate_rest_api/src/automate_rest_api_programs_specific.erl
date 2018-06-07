%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_programs_specific).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).
-export([to_json/2]).

-include("./records.hrl").

-record(get_program_seq, { username, program_name }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramName = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #get_program_seq{ username=UserId
                      , program_name=ProgramName
                      }}.

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
                    #get_program_seq{username=Username} = State,
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

%% Get handler
content_types_provided(Req, State) ->
    io:fwrite("User > program > ID~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #get_program_seq{})
                                   -> {'true',cowboy_req:req(),_}.
to_json(Req, State) ->
    #get_program_seq{username=Username, program_name=ProgramName} = State,
    case automate_rest_api_backend:get_program(Username, ProgramName) of
        { ok, Program } ->

            Output = program_to_json(Program),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


program_to_json(#user_program{ id=Id
                             , user_id=UserId
                             , program_name=ProgramName
                             , program_type=ProgramType
                             , program_content=ProgramContent
                             }) ->
    jiffy:encode(#{ <<"id">> => Id
                  , <<"owner">> => UserId
                  , <<"name">> => ProgramName
                  , <<"type">> => ProgramType
                  , <<"content">> => ProgramContent
                  }).
