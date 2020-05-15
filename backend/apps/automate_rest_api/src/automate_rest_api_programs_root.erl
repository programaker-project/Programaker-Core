%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_programs_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_json_create_program/2
        , to_json/2
        ]).

-include("./records.hrl").
-define(UTILS, automate_rest_api_utils).
-define(DEFAULT_PROGRAM_TYPE, scratch_program).

-record(create_program_seq, { username }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #create_program_seq{ username=UserId }}.

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
    {[{{<<"application">>, <<"json">>, []}, accept_json_create_program}],
     Req, State}.

-spec accept_json_create_program(cowboy_req:req(), #create_program_seq{})
                                -> {{'true', binary()},cowboy_req:req(), #create_program_seq{}}.
accept_json_create_program(Req, State) ->
    #create_program_seq{username=Username} = State,

    {Type, Name} = try
                       {ok, Body, _} = ?UTILS:read_body(Req),
                       jiffy:decode(Body, [return_maps])
                   of
                       Map ->
                           { get_program_type_from_options(Map)
                           , get_program_name_from_options(Map)
                           }
                   catch _:_ ->
                           { ?DEFAULT_PROGRAM_TYPE, generate_program_name() }
             end,
    case automate_rest_api_backend:create_program(Username, Name, Type) of
        { ok, {ProgramId, ProgramName, ProgramUrl, ProgramType} } ->

            Output = jiffy:encode(#{ <<"id">> => ProgramId
                                   , <<"name">> => ProgramName
                                   , <<"link">> =>  ProgramUrl
                                   , <<"type">> => ProgramType
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { {true, ProgramUrl }, Res3, State }
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #create_program_seq{})
             -> {binary(),cowboy_req:req(), #create_program_seq{}}.
to_json(Req, State) ->
    #create_program_seq{username=Username} = State,
    case automate_rest_api_backend:lists_programs_from_username(Username) of
        { ok, Programs } ->
            Output = jiffy:encode(encode_program_list(Programs)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


encode_program_list(Programs) ->
    lists:map(fun(#program_metadata{ id=Id
                                   , name=Name
                                   , link=Link
                                   , enabled=Enabled
                                   , type=Type
                                   }) ->
                      #{ <<"id">> => Id
                       , <<"name">> => Name
                       , <<"link">> =>  Link
                       , <<"enabled">> => Enabled
                       , <<"type">> => Type
                       , <<"bridges_in_use">> => try get_bridges_on_program_id(Id) of
                                                     Bridges -> Bridges
                                                 catch ErrNS:Error:StackTrace ->
                                                         automate_logging:log_platform(error, ErrNS, Error, StackTrace),
                                                         []
                                                 end
                       }
              end, Programs).

get_bridges_on_program_id(ProgramId) ->
    {ok, Program} = automate_storage:get_program_from_id(ProgramId),
    {ok, Bridges} = automate_bot_engine:get_bridges_on_program(Program),
    Bridges.

% Util functions
get_program_type_from_options(#{ <<"type">> := <<"scratch_program">> }) ->
    scratch_program;
get_program_type_from_options(#{ <<"type">> := <<"flow_program">> }) ->
    flow_program;
get_program_type_from_options(#{ <<"type">> := Type }) when is_binary(Type) ->
    Type;
get_program_type_from_options(_) ->
    ?DEFAULT_PROGRAM_TYPE.

get_program_name_from_options(#{ <<"name">> := Name }) when is_binary(Name) ->
    case size(Name) < 4 of
        true ->
            generate_program_name();
        false ->
            Name
    end;
get_program_name_from_options(_) ->
    generate_program_name().

generate_program_name() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
