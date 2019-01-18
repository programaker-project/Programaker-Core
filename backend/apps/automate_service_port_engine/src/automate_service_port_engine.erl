%%%-------------------------------------------------------------------
%% @doc automate_service_port_engine APP API
%% @end
%%%-------------------------------------------------------------------

%% @doc automate_service_port_engine APP API
-module(automate_service_port_engine).

%% Application callbacks
-export([ create_service_port/2
        , register_service_port/1
        , from_service_port/3
        , call_service_port/3
        ]).

-include("records.hrl").

-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(ROUTER, automate_service_port_engine_router).

%%====================================================================
%% API
%%====================================================================

-spec create_service_port(binary(), boolean()) -> {ok, binary()} | {error, term(), string()}.
create_service_port(UserId, ServicePortName) ->
    ?BACKEND:create_service_port(UserId, ServicePortName).

-spec register_service_port(binary()) -> ok.
register_service_port(ServicePortId) ->
    ChannelId = ServicePortId,
    Process = self(),
    io:fwrite("Connected service port ~p~n", [ServicePortId]),
    ?ROUTER:open_outbound_channel({to_service, ChannelId},
                                  fun(Msg) ->
                                          Process ! Msg,
                                          continue
                                  end),
    ok.

-spec call_service_port(binary(), binary(), binary()) -> {ok, any()}.
call_service_port(ServicePortId, FunctionName, Arguments) ->
    ChannelId = ServicePortId,
    Process = self(),

    MessageId = generate_id(),

    ?ROUTER:open_outbound_channel({from_service, ChannelId},
                                  fun(Answer=#{ <<"message_id">> := ReceivedMessageId
                                              }) ->
                                          case ReceivedMessageId of
                                              MessageId ->
                                                  Process ! {?MODULE, Answer},
                                                  finish;
                                              _ ->
                                                  continue
                                          end
                                  end),
    ?ROUTER:route_inbound({to_service, ChannelId},
                          jiffy:encode(#{ <<"message_id">> => MessageId
                                        , <<"type">> => <<"FUNCTION_CALL">>
                                        , <<"value">> => #{ <<"function_name">> => FunctionName
                                                          , <<"arguments">> => Arguments
                                                          }
                                        })),

    receive {?MODULE, Msg} ->
            {ok, Msg}
    end.

-spec from_service_port(binary(), binary(), binary()) -> ok.
from_service_port(ServicePortId, UserId, Msg) ->
    ChannelId = ServicePortId,
    Unpacked = jiffy:decode(Msg, [return_maps]),
    case Unpacked of
        #{ <<"message_id">> := _ } ->
            io:fwrite("Service answered ~p~n", [Unpacked]),
            ?ROUTER:route_inbound({from_service, ChannelId}, Unpacked),
            ok;
        #{ <<"type">> := <<"configuration">>
         , <<"value">> := Configuration
         } ->
            set_service_port_configuration(ServicePortId, Configuration, UserId)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

set_service_port_configuration(ServicePortId, Configuration, UserId) ->
    SPConfiguration = parse_configuration_map(ServicePortId, Configuration),
    ?BACKEND:set_service_port_configuration(ServicePortId, SPConfiguration, UserId),
    ok.

parse_configuration_map(ServicePortId,
                        #{ <<"blocks">> := Blocks
                         , <<"is_public">> := IsPublic
                         , <<"service_name">> := ServiceName
                         }) ->
    #service_port_configuration{ id=ServicePortId
                               , is_public=IsPublic
                               , service_id=undefined
                               , service_name=ServiceName
                               , blocks=lists:map(fun(B) -> parse_block(B) end, Blocks)
                               }.

parse_block(#{ <<"arguments">> := Arguments
             , <<"function_name">> := FunctionName
             , <<"message">> := Message
             , <<"id">> := BlockId
             }) ->
    #service_port_block{ block_id=BlockId
                       , function_name=FunctionName
                       , message=Message
                       , arguments=lists:map(fun parse_argument/1, Arguments)
                       }.

parse_argument(#{ <<"default">> := DefaultValue
                , <<"type">> := Type
                }) ->
    #service_port_block_argument{ default=DefaultValue
                                , type=Type
                                }.

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
