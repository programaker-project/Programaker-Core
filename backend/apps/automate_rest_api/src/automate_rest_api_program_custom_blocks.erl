%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_custom_blocks).
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
-include("../../automate_storage/src/records.hrl").

-record(state, { program_id :: binary()
               , owner :: owner_id() | undefined
               }).
-define(FORMATTING, automate_rest_api_utils_formatting).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #state{ program_id=ProgramId }
    }.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            {ok, #user_program_entry{ owner=Owner }} = automate_storage:get_program_from_id(ProgramId),
                            case automate_storage:can_user_view_as({user, UserId}, Owner) of
                                true -> { true, Req1, State#state{ owner=Owner } };
                                false ->
                                    { { false, <<"Operation not allowed">>}, Req1, State }
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
to_json(Req, State=#state{owner=Owner}) ->
    case automate_service_port_engine:list_custom_blocks(Owner) of
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
     , <<"save_to">> => ?FORMATTING:serialize_maybe_undefined(SaveTo)
     };

encode_block(#service_port_trigger_block{ block_id=BlockId
                                        , function_name=FunctionName
                                        , message=Message
                                        , arguments=Arguments
                                        , block_type=BlockType
                                        , save_to=SaveTo
                                        , expected_value=ExpectedValue
                                        , key=Key
                                        , subkey=SubKey
                                        }) ->
    #{ <<"block_id">> => BlockId
     , <<"function_name">> => FunctionName
     , <<"message">> => Message
     , <<"arguments">> => lists:map(fun encode_argument/1, Arguments)
     , <<"block_type">> => BlockType
     , <<"save_to">> => ?FORMATTING:serialize_maybe_undefined(SaveTo)
     , <<"expected_value">> => ExpectedValue
     , <<"key">> => ?FORMATTING:serialize_maybe_undefined(Key)
     , <<"subkey">> => ?FORMATTING:serialize_maybe_undefined(SubKey)
     };

%% TODO: Add DB migration to avoid the need of this compatibility
encode_block({service_port_trigger_block
             , BlockId
             , FunctionName
             , Message
             , Arguments
             , BlockType
             , SaveTo
             , ExpectedValue
             , Key
             }) ->
    #{ <<"block_id">> => BlockId
     , <<"function_name">> => FunctionName
     , <<"message">> => Message
     , <<"arguments">> => lists:map(fun encode_argument/1, Arguments)
     , <<"block_type">> => BlockType
     , <<"save_to">> => ?FORMATTING:serialize_maybe_undefined(SaveTo)
     , <<"expected_value">> => ExpectedValue
     , <<"key">> => ?FORMATTING:serialize_maybe_undefined(Key)
     , <<"subkey">> => null
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
     };

encode_argument(#service_port_block_dynamic_sequence_argument{ type=Type
                                                             , callback_sequence=CallbackSequence
                                                             }) ->
    #{ <<"type">> => Type
     , <<"callback_sequence">> => CallbackSequence
     };

encode_argument(#service_port_block_collection_argument{ name=Collection
                                                       }) ->
    #{ type => string
     , callback => Collection
     , collection => Collection
     }.
