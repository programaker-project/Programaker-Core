%%%-------------------------------------------------------------------
%% @doc automate_service_port_engine APP API
%% @end
%%%-------------------------------------------------------------------

%% @doc automate_service_port_engine APP API
-module(automate_service_port_engine).

%% Application callbacks
-export([ create_service_port/2
        , register_service_port/1
        , from_service_port/2
        , call_service_port/3
        ]).

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
    ?ROUTER:open_outbound_channel({to_service, ChannelId},
                                  fun(Msg) ->
                                          Process ! Msg
                                  end),
    ok.

-spec call_service_port(binary(), binary(), binary()) -> {ok, any()}.
call_service_port(ServicePortId, FunctionName, Arguments) ->
    ChannelId = ServicePortId,
    Process = self(),

    MessageId = generate_id(),

    Connection = ?ROUTER:open_outbound_channel({from_service, ChannelId},
                                  fun(Answer=#{ <<"message_id">> := ReceivedMessageId
                                              }) ->
                                          case ReceivedMessageId of
                                              MessageId ->
                                                  Process ! {?MODULE, Answer};
                                              _ ->
                                                  ok
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
            ?ROUTER:close_outbound_channel(Connection),
            {ok, Msg}
    end.

-spec from_service_port(binary(), binary()) -> ok.
from_service_port(ServicePortId, Msg) ->
    ChannelId = ServicePortId,
    Unpacked = jiffy:decode(Msg, [return_maps]),
    io:fwrite("Service answered ~p~n", [Unpacked]),
    ?ROUTER:route_inbound({from_service, ChannelId}, Unpacked),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
