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
        , call_service_port/5
        , get_how_to_enable/2
        , send_registration_data/3
        , send_oauth_return/2

        , list_custom_blocks/1
        , internal_user_id_to_connection_id/2
        , get_user_service_ports/1
        , delete_bridge/2
        , callback_bridge/3
        , get_channel_origin_bridge/1
        , get_bridge_info/1

        , listen_bridge/2
        , list_established_connections/1
        , get_pending_connection_info/1
        , is_module_connectable_bridge/2
        ]).

-include("records.hrl").

-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(ROUTER, automate_service_port_engine_router).
-define(LOGGING, automate_logging).
-include("router_error_cases.hrl").

%%====================================================================
%% API
%%====================================================================

-spec create_service_port(binary(), binary()) -> {ok, binary()} | {error, term(), string()}.
create_service_port(UserId, ServicePortName) ->
    ?BACKEND:create_service_port(UserId, ServicePortName).

-spec register_service_port(binary()) -> ok.
register_service_port(ServicePortId) ->
    ?ROUTER:connect_bridge(ServicePortId).

-spec call_service_port(binary(), binary(), any(), binary(), map()) -> {ok, map()} | {error, ?ROUTER_ERROR_CLASSES}.
call_service_port(ServicePortId, FunctionName, Arguments, UserId, ExtraData) ->
    ?LOGGING:log_call_to_bridge(ServicePortId,FunctionName,Arguments,UserId,ExtraData),
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"FUNCTION_CALL">>
                                        , <<"user_id">> => UserId
                                        , <<"value">> => #{ <<"function_name">> => FunctionName
                                                          , <<"arguments">> => Arguments
                                                          }
                                        , <<"extra_data">> => ExtraData
                                        }).

-spec get_how_to_enable(binary(), binary()) -> {ok, any()}.
get_how_to_enable(ServicePortId, UserId) ->
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"GET_HOW_TO_SERVICE_REGISTRATION">>
                                        , <<"user_id">> => UserId
                                        , <<"value">> => #{}
                                        }).

-spec send_registration_data(binary(), map(), binary()) -> {ok, map()}.
send_registration_data(ServicePortId, Data, UserId) ->
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"REGISTRATION">>
                                        , <<"user_id">> => UserId
                                        , <<"value">> => #{ <<"form">> => Data }
                                        }).

-spec send_oauth_return(binary(), binary()) -> {ok, map()} | {error, term()}.
send_oauth_return(Qs, ServicePortId) ->
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"OAUTH_RETURN">>
                                        , <<"value">> => #{ <<"query_string">> => Qs }
                                        }).

-spec listen_bridge(binary(), binary()) -> ok | {error, term()}.
listen_bridge(BridgeId, UserId) ->
    case ?BACKEND:get_or_create_monitor_id(UserId, BridgeId) of
        { ok, ChannelId } ->
            automate_channel_engine:listen_channel(ChannelId);
        {error, _X, Description} ->
            {error, Description}
    end.

-spec from_service_port(binary(), binary(), binary()) -> ok.
from_service_port(ServicePortId, UserId, Msg) ->
    Unpacked = jiffy:decode(Msg, [return_maps]),
    automate_stats:log_observation(counter,
                                   automate_bridge_engine_messages_from_bridge,
                                   [ServicePortId]),
    case Unpacked of
        %% This has to be first because of the use of MessageId here
        AdviceMsg = #{ <<"type">> := <<"ADVICE_SET">>, <<"message_id">> := MessageId } ->
            AdviceTaken = apply_advice(AdviceMsg, ServicePortId),
            answer_advice_taken(AdviceTaken, MessageId, self());

        #{ <<"message_id">> := MessageId } ->
            %% io:fwrite("[~p] Answer: ~p~n", [MessageId, Unpacked]),
            ?ROUTER:answer_message(MessageId, Unpacked);

        #{ <<"type">> := <<"CONFIGURATION">>
         , <<"value">> := Configuration
         } ->
            {ok, Todo} = set_service_port_configuration(ServicePortId, Configuration, UserId),
            %% TODO: Check that it really exists, don't trust the DB
            case lists:member(request_icon, Todo) of
                false -> ok;
                true ->
                    %% Request icon
                    request_icon(self())
            end;

        #{ <<"type">> := <<"ICON_UPLOAD">>
         , <<"value">> := IconData
         } ->
            case IconData of
                #{ <<"content">> := B64Content } ->
                    Data = base64:decode(B64Content),
                    ok = write_icon(Data, ServicePortId)
            end;

        #{ <<"type">> := <<"ESTABLISH_CONNECTION">>
         , <<"value">> := #{ <<"connection_id">> := ConnectionId
                           , <<"name">> := Name
                           }
         } ->
            case ?BACKEND:establish_connection(ServicePortId, ConnectionId, Name) of
                ok ->
                    io:fwrite("[~p] Established connection: ~p~n", [ServicePortId, ConnectionId]);
                {error, Reason} ->
                    io:fwrite("[~p] Tried to establish connection but failed: ~p~n", [ServicePortId, Reason])
            end;

        #{ <<"type">> := <<"NOTIFICATION">>
         , <<"key">> := Key
         , <<"to_user">> := ToUser
         , <<"value">> := Value
         , <<"content">> := Content
         } ->
            case ToUser of
                null ->
                    %% TODO: This looping be removed if the users also listened on
                    %% a common bridge channel. For this, the service API should allow
                    %% returning multiple channels when asked.
                    {ok, Channels} = ?BACKEND:list_bridge_channels(ServicePortId),
                    Results = lists:map(fun (Channel) ->
                                                { Channel
                                                , automate_channel_engine:send_to_channel(
                                                    Channel,
                                                    #{ <<"key">> => Key
                                                     , <<"value">> => Value
                                                     , <<"content">> => Content
                                                     })}
                                        end, Channels),
                    lists:foreach(
                      fun({Channel, Result}) ->
                              case Result of
                                  ok -> ok;
                                  Err -> io:fwrite("Error sending to channel (~p): ~p~n", [Channel, Err])
                              end
                      end, Results),
                    %% Make sure to crash if there's an error, but only after the
                    %% messages had been sent
                    ok;
                _ ->
                    case ?BACKEND:connection_id_to_internal_user_id(
                                                ToUser, ServicePortId) of
                        {ok, ToUserInternalId} ->
                            {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(
                                                            ServicePortId, ToUserInternalId),

                            {ok, MonitorId } = automate_service_registry_query:get_monitor_id(
                                                 Module, ToUserInternalId),
                            ok = automate_channel_engine:send_to_channel(MonitorId,
                                                                         #{ <<"key">> => Key
                                                                          , <<"value">> => Value
                                                                          , <<"content">> => Content
                                                                          });
                        {error, Reason} ->
                            io:fwrite("[~p] Error propagating notification (to ~p): ~p~n", [ServicePortId, ToUser, Reason])
                    end
            end
    end.

-spec list_custom_blocks(binary()) -> {ok, map()}.
list_custom_blocks(UserId) ->
    ?BACKEND:list_custom_blocks(UserId).

-spec internal_user_id_to_connection_id(binary(), binary()) -> {ok, binary()} | {error, not_found}.
internal_user_id_to_connection_id(UserId, ServicePortId) ->
    ?BACKEND:internal_user_id_to_connection_id(UserId, ServicePortId).


-spec get_user_service_ports(binary()) -> {ok, [#service_port_entry_extra{}]}.
get_user_service_ports(UserId) ->
    {ok, Bridges} = ?BACKEND:get_user_service_ports(UserId),
    {ok, lists:map(fun add_service_port_extra/1, Bridges)}.

-spec delete_bridge(binary(), binary()) -> ok | {error, binary()}.
delete_bridge(UserId, BridgeId) ->
    ok = case ?BACKEND:get_service_id_for_port(BridgeId) of
             {error, not_found} ->
                 ok;
             {ok, ServiceId} ->
                 automate_service_registry:delete_service(UserId, ServiceId)
         end,
    ?BACKEND:delete_bridge(UserId, BridgeId).


-spec callback_bridge(binary(), binary(), binary()) -> {ok, map()} | {error, term()}.
callback_bridge(UserId, BridgeId, Callback) ->
    {ok, BridgeUserId} = internal_user_id_to_connection_id(UserId, BridgeId),
    ?ROUTER:call_bridge(BridgeId, #{ <<"type">> => <<"CALLBACK">>
                                   , <<"user_id">> => BridgeUserId
                                   , <<"value">> => #{ <<"callback">> => Callback
                                                     }
                                   }).


-spec get_channel_origin_bridge(binary()) -> {ok, binary()} | {error, not_found}.
get_channel_origin_bridge(ChannelId) ->
    case automate_services_time:get_monitor_id(none) of
        {ok, ChannelId} ->
            {ok, automate_services_time:get_uuid()};
        _ ->
            ?BACKEND:get_channel_origin_bridge(ChannelId)
    end.

-spec get_bridge_info(binary()) -> {ok, #service_port_metadata{}} | {error, not_found}.
get_bridge_info(BridgeId) ->
    ?BACKEND:get_bridge_info(BridgeId).

-spec list_established_connections(binary()) -> {ok, [#user_to_bridge_connection_entry{}]}.
list_established_connections(UserId) ->
    {ok, _Connections} = ?BACKEND:list_established_connections(UserId).

-spec get_pending_connection_info(binary()) -> {ok, #user_to_bridge_pending_connection_entry{}}.
get_pending_connection_info(ConnectionId) ->
    ?BACKEND:get_pending_connection_info(ConnectionId).


-spec is_module_connectable_bridge(binary(), module() | {module(), any()}) ->
          false | {boolean(), {#service_port_entry{}, #service_port_configuration{}}}.
is_module_connectable_bridge(UserId, {automate_service_port_engine_service, [ BridgeId | _ ]}) ->
    %% It *is* a bridge. Only remains to check if a new connection can be established.
    {ok, BridgeInfo, BridgeConfiguration} = ?BACKEND:get_all_bridge_info(BridgeId),
    IsConnectable = case BridgeConfiguration of
                        undefined -> false;
                        #service_port_configuration{ allow_multiple_connections=true } ->
                            true;
                        #service_port_configuration{ allow_multiple_connections=false } ->
                            {ok, Connected} = ?BACKEND:is_user_connected_to_bridge(UserId, BridgeId),
                            not Connected
                    end,
    {IsConnectable, {BridgeInfo, BridgeConfiguration}};

is_module_connectable_bridge(_, _) ->
    %% Is not a bridge
    false.



%%====================================================================
%% Internal functions
%%====================================================================

-spec add_service_port_extra({#service_port_entry{}, #service_port_configuration{}}) -> #service_port_entry_extra{}.
add_service_port_extra({#service_port_entry{ id=Id
                                           , name=Name
                                           , owner=Owner
                                           , service_id=ServiceId
                                           }, Config}) ->
    {ok, IsConnected} = ?ROUTER:is_bridge_connected(Id),

    BridgeIcon = case Config of
                     undefined -> undefined;
                     #service_port_configuration{ icon=Icon } -> Icon
                 end,
    #service_port_entry_extra{ id=Id
                             , name=Name
                             , owner=Owner
                             , service_id=ServiceId
                             , is_connected=IsConnected
                             , icon=BridgeIcon
                             }.

set_service_port_configuration(ServicePortId, Configuration, UserId) ->
    SPConfiguration = parse_configuration_map(ServicePortId, Configuration),
    ?BACKEND:set_service_port_configuration(ServicePortId, SPConfiguration, UserId).

parse_configuration_map(ServicePortId,
                        Config=#{ <<"blocks">> := Blocks
                                , <<"is_public">> := IsPublic
                                , <<"service_name">> := ServiceName
                                }) ->
    #service_port_configuration{ id=ServicePortId
                               , is_public=IsPublic
                               , service_id=undefined
                               , service_name=ServiceName
                               , blocks=lists:map(fun(B) -> parse_block(B) end, Blocks)
                               , icon=get_icon_from_config(Config)
                               , allow_multiple_connections=get_allow_multiple_connections_from_config(Config)
                               }.


-spec get_icon_from_config(map()) -> undefined | supported_icon_type().
get_icon_from_config(#{ <<"icon">> := #{ <<"url">> := Url } }) ->
    { url, Url };
get_icon_from_config(#{ <<"icon">> := #{ <<"sha256">> := Hash } }) ->
    Id={ hash, sha256, Hash },
    %% TODO: Check that ID exists, or request to bridge
    Id;
get_icon_from_config(_) ->
    undefined.

-spec get_allow_multiple_connections_from_config(map()) -> boolean().
get_allow_multiple_connections_from_config(#{ <<"allow_multiple_connections">> := AllowMultipleConnections })
  when is_boolean(AllowMultipleConnections) ->
    AllowMultipleConnections;
get_allow_multiple_connections_from_config(_) ->
    false.



parse_block(Block=#{ <<"arguments">> := Arguments
                   , <<"function_name">> := FunctionName
                   , <<"message">> := Message
                   , <<"id">> := BlockId
                   , <<"block_type">> := BlockType
                   , <<"block_result_type">> := BlockResultType
                   }) ->
    #service_port_block{ block_id=BlockId
                       , function_name=FunctionName
                       , message=Message
                       , arguments=lists:map(fun parse_argument/1, Arguments)
                       , block_type=BlockType
                       , block_result_type=BlockResultType
                       , save_to=get_block_save_to(Block)
                       };

parse_block(Block=#{ <<"arguments">> := Arguments
                             , <<"function_name">> := FunctionName
                             , <<"message">> := Message
                             , <<"id">> := BlockId
                             , <<"block_type">> := BlockType
                             , <<"save_to">> := SaveToConfiguration
                             , <<"expected_value">> := ExpectedValue
                             , <<"key">> := Key
                             }) ->
    #service_port_trigger_block{ block_id=BlockId
                               , function_name=FunctionName
                               , message=Message
                               , arguments=lists:map(fun parse_argument/1, Arguments)
                               , block_type=BlockType
                               , save_to=SaveToConfiguration
                               , expected_value=ExpectedValue
                               , key=Key
                               , subkey=get_block_subkey(Block)
                               }.

get_block_save_to(#{ <<"save_to">> := SaveTo }) ->
    SaveTo;
get_block_save_to(_) ->
    undefined.


get_block_subkey(#{ <<"subkey">> := SubKey }) ->
    SubKey;
get_block_subkey(_) ->
    undefined.


parse_argument(#{ <<"default">> := DefaultValue
                , <<"type">> := Type
                }) ->
    #service_port_block_static_argument{ default=DefaultValue
                                       , type=Type
                                       , class=undefined
                                       };

parse_argument(#{ <<"type">> := <<"variable">>
                , <<"class">> := Class
                }) ->
    #service_port_block_static_argument{ type= <<"variable">>
                                       , class=Class
                                       , default=undefined
                                       };

parse_argument(#{ <<"type">> := <<"variable">>
                }) ->
    #service_port_block_static_argument{ type= <<"variable">>
                                       , default=undefined
                                       , class=undefined
                                       };

parse_argument(#{ <<"type">> := Type
                , <<"values">> := #{ <<"callback">> := Callback
                                   }
                }) ->
    #service_port_block_dynamic_argument{ callback=Callback
                                        , type=Type
                                        }.

answer_advice_taken(AdviceTaken, MessageId, Pid) ->
    Pid ! {{ automate_service_port_engine, advice_taken}, MessageId, AdviceTaken }.

request_icon(Pid) ->
    Pid ! {{ automate_service_port_engine, request_icon} }.

get_icon_path(ServicePortId) ->
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s", [automate_configuration:asset_directory("public/icons")
                                           , ServicePortId
                                           ]))).

write_icon(Data, ServicePortId) ->
    file:write_file(get_icon_path(ServicePortId), Data).

apply_advice(#{ <<"type">> := <<"ADVICE_SET">>
              , <<"value">> := Value
              }, BridgeId) ->
    lists:filtermap(fun ({Advice, Content}) ->
                            case apply_advice(Advice, Content, BridgeId) of
                                true ->
                                    {true, Advice};
                                false ->
                                    false
                            end
                    end, maps:to_list(Value)).

apply_advice(<<"NOTIFY_SIGNAL_LISTENERS">>, Content, BridgeId) ->
    ?BACKEND:set_notify_signal_listeners(Content, BridgeId),
    {ok, Listeners} = ?BACKEND:get_signal_listeners(Content, BridgeId),
    report_current_listeners(self(), Listeners),
    true;

apply_advice(Advice, _Content, _BridgeId) ->
    io:fwrite("[~p] Unknown advice: ~p~n", [self(), Advice]),
    false.

report_current_listeners(Reported, Listeners) ->
    lists:foreach(fun ({ Pid, Key, SubKey }) ->
                          Reported ! { automate_channel_engine, add_listener, { Pid, Key, SubKey } }
                  end, Listeners).
