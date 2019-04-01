%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_custom_blocks_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { username }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #state{ username=UserId }}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

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
                    #state{username=Username} = State,
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

%% GET handler
content_types_provided(Req, State) ->
    io:fwrite("Control types provided~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{username=Username} = State,
    case automate_rest_api_backend:list_custom_blocks_from_username(Username) of
        { ok, CustomBlocks } ->

            Output = jiffy:encode(maps:map(fun encode_blocks/2, CustomBlocks)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.

encode_blocks(_K, Blocks) ->
    lists:map(fun encode_block/1, Blocks).

encode_block(#service_port_block{ block_id=BlockId
                                , function_name=FunctionName
                                , message=Message
                                , arguments=Arguments
                                , block_type=BlockType
                                , block_result_type=BlockResultType
                                , save_to=SaveTo
                                }) ->
    #{ <<"block_id">> => BlockId
     , <<"function_name">> => FunctionName
     , <<"message">> => Message
     , <<"arguments">> => lists:map(fun encode_argument/1, Arguments)
     , <<"block_type">> => BlockType
     , <<"block_result_type">> => BlockResultType
     , <<"save_to">> => SaveTo
     };

encode_block(#service_port_trigger_block{ block_id=BlockId
                                        , function_name=FunctionName
                                        , message=Message
                                        , arguments=Arguments
                                        , block_type=BlockType
                                        , save_to=SaveTo
                                        , expected_value=ExpectedValue
                                        , key=Key
                                        }) ->
    #{ <<"block_id">> => BlockId
     , <<"function_name">> => FunctionName
     , <<"message">> => Message
     , <<"arguments">> => lists:map(fun encode_argument/1, Arguments)
     , <<"block_type">> => BlockType
     , <<"save_to">> => SaveTo
     , <<"expected_value">> => ExpectedValue
     , <<"key">> => Key
     }.

encode_argument(#service_port_block_static_argument{ type=Type
                                                   , default=Default
                                                   , class=Class
                                                   }) ->
    #{ <<"type">> => Type
     , <<"default_value">> => Default
     , <<"class">> => Class
     };

encode_argument(#service_port_block_dynamic_argument{ type=Type
                                                    , callback=Callback
                                                    }) ->
    #{ <<"type">> => Type
     , <<"callback">> => Callback
     }.
