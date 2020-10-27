%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_mnesia_backend).

-export([ start_link/0
        , create_service_port/2
        , set_service_port_configuration/3
        , set_notify_signal_listeners/2
        , get_signal_listeners/2

        , list_custom_blocks/1
        , get_block_definition/2
        , internal_user_id_to_connection_id/2
        , is_user_connected_to_bridge/2
        , connection_id_to_internal_user_id/2
        , get_user_service_ports/1
        , list_bridge_channels/1
        , list_bridge_connections/1
        , list_established_connections/1
        , list_established_connections/2
        , get_connection_owner/1
        , get_pending_connection_info/1

        , gen_pending_connection/2
        , establish_connection/4
        , establish_connection/3

        , get_service_id_for_port/1
        , get_bridge_info/1
        , get_bridge_owner/1
        , get_bridge_configuration/1
        , get_all_bridge_info/1
        , delete_bridge/2

        , get_or_create_monitor_id/2
        , uninstall/0
        , get_channel_origin_bridge/1

        , set_shared_resource/3
        , get_connection_shares/1
        , get_resources_shared_with/1
        , get_connection_bridge/1

        , create_bridge_token/4
        , list_bridge_tokens/1
        , delete_bridge_token_by_name/2
        , check_bridge_token/2
        , can_skip_authentication/1
        , set_save_signals_from_bridge/3
        , check_save_signals_in_connection/1
        ]).

-include("records.hrl").
-include("databases.hrl").
-include("../../automate_storage/src/security_params.hrl").

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),

    ok = automate_storage_versioning:apply_versioning(automate_service_port_engine_configuration:get_versioning(Nodes),
                                                      Nodes, ?MODULE),

    %% These are run on RAM, so they are created manually
    ok = case mnesia:create_table(?SERVICE_PORT_CHANNEL_MONITORS_TABLE,
                                  [ { attributes, record_info(fields, channel_monitor_table_entry)}
                                  , { ram_copies, Nodes }
                                  , { record_name, channel_monitor_table_entry }
                                  , { type, bag }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    ignore.


-spec uninstall() -> ok.
uninstall() ->
    {atomic, ok} = mnesia:delete_table(?SERVICE_PORT_TABLE),
    {atomic, ok} = mnesia:delete_table(?SERVICE_PORT_CONFIGURATION_TABLE),
    {atomic, ok} = mnesia:delete_table(?SERVICE_PORT_CHANNEL_TABLE),
    ok.

-spec create_service_port(owner_id(), binary()) -> {ok, binary()} | {error, _, string()}.
create_service_port(Owner, ServicePortName) ->
    ServicePortId = generate_id(),
    Entry = #service_port_entry{ id=ServicePortId
                               , name=ServicePortName
                               , owner=Owner
                               , old_skip_authentication=false %% Only old bridges can skip authentication
                               },

    Transaction = fun() ->
                          ok = mnesia:write(?SERVICE_PORT_TABLE, Entry, write)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            {ok, ServicePortId};
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec gen_pending_connection(binary(), owner_id()) -> {ok, binary()}.
gen_pending_connection(BridgeId, Owner) ->
    ConnectionId = generate_id(),
    CurrentTime = erlang:system_time(second),

    Transaction = fun() ->
                          {ok, ChannelId} = automate_channel_engine:create_channel(),
                          Entry = #user_to_bridge_pending_connection_entry{ id=ConnectionId
                                                                          , bridge_id=BridgeId
                                                                          , owner=Owner
                                                                          , channel_id=ChannelId
                                                                          , creation_time=CurrentTime
                                                                          },
                          ok = mnesia:write(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, Entry, write),
                          {ok, ConnectionId}
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

%% Establish connection confirming Bridge and User id
-spec establish_connection(binary(), owner_id(), binary(), binary()) -> ok | {error, not_found}.
establish_connection(BridgeId, Owner, ConnectionId, Name) ->
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:read(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId) of
                              [] ->
                                  {error, not_found};
                              [ #user_to_bridge_pending_connection_entry{ bridge_id=BridgeId
                                                                        , owner=Owner
                                                                        , channel_id=ChannelId
                                                                        } ] ->
                                  ok = mnesia:delete(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId, write),

                                  Entry = #user_to_bridge_connection_entry{ id=ConnectionId
                                                                          , bridge_id=BridgeId
                                                                          , owner=Owner
                                                                          , channel_id=ChannelId
                                                                          , name=Name
                                                                          , creation_time=CurrentTime
                                                                          , save_signals=false
                                                                          },
                                  ok = mnesia:write(?USER_TO_BRIDGE_CONNECTION_TABLE, Entry, write),
                                  {ok, ChannelId}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, {ok, ChannelId}} ->
            ok = automate_channel_engine:send_to_channel(ChannelId, connection_established),
            ok;
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% Establish connection confirming Bridge id and recovering user.
-spec establish_connection(binary(), binary(), binary()) -> ok | {error, not_found}.
establish_connection(BridgeId, ConnectionId, Name) ->
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:read(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId) of
                              [] ->
                                  {error, not_found};
                              [ #user_to_bridge_pending_connection_entry{ bridge_id=BridgeId
                                                                        , owner=Owner
                                                                        , channel_id=ChannelId
                                                                        } ] ->
                                  ok = mnesia:delete(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId, write),

                                  Entry = #user_to_bridge_connection_entry{ id=ConnectionId
                                                                          , bridge_id=BridgeId
                                                                          , owner=Owner
                                                                          , channel_id=ChannelId
                                                                          , name=Name
                                                                          , creation_time=CurrentTime
                                                                          , save_signals=false
                                                                          },
                                  ok = mnesia:write(?USER_TO_BRIDGE_CONNECTION_TABLE, Entry, write),
                                  {ok, ChannelId}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, {ok, ChannelId}} ->
            ok = automate_channel_engine:send_to_channel(ChannelId, connection_established),
            ok;
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

get_service_id_for_port(ServicePortId) ->
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, ServicePortId) of
                              [] ->
                                  {error, not_found};
                              [#service_port_configuration{service_id=ServiceId}] ->
                                  {ok, ServiceId}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.


-spec get_bridge_info(binary()) -> {ok, #service_port_metadata{}} | {error, not_found}.
get_bridge_info(BridgeId) ->
    case get_all_bridge_info(BridgeId) of
        { ok, #service_port_entry{name=Name, owner=Owner} , undefined} ->
            {ok, #service_port_metadata{ id=BridgeId
                                       , name=Name
                                       , owner=Owner
                                       , icon=undefined
                                       }};
        { ok, #service_port_entry{name=Name, owner=Owner} , #service_port_configuration{ icon=Icon }} ->
            { ok, #service_port_metadata{ id=BridgeId
                                        , name=Name
                                        , owner=Owner
                                        , icon=Icon
                                        } } ;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_bridge_owner(binary()) -> {ok, owner_id()} | {error, not_found}.
get_bridge_owner(BridgeId) ->
    T = fun() ->
                case mnesia:read(?SERVICE_PORT_TABLE, BridgeId) of
                    [] ->
                        {error, not_found};
                    [#service_port_entry{ owner=Owner }] ->
                        {ok, Owner}
                end
        end,
    automate_storage:wrap_transaction(mnesia:activity(ets, T)).


-spec get_bridge_configuration(binary()) -> {ok, #service_port_configuration{}} | {error, not_found}.
get_bridge_configuration(BridgeId) ->
    T = fun() ->
                case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, BridgeId) of
                    [] ->
                        {error, not_found};
                    [Entry] ->
                        {ok, Entry}
                end
        end,
    automate_storage:wrap_transaction(mnesia:activity(ets, T)).


-spec get_all_bridge_info(binary()) -> {ok, #service_port_entry{}, undefined | #service_port_configuration{}} | {error, _}.
get_all_bridge_info(BridgeId) ->
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_PORT_TABLE, BridgeId) of
                              [] ->
                                  {error, not_found};
                              [Entry] ->
                                  case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, BridgeId) of
                                      [] ->
                                          { ok, Entry, undefined };
                                      [Config] ->
                                          { ok, Entry, Config }
                                  end
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.


-spec create_service_for_port(#service_port_configuration{}, owner_id()) -> {ok, binary()}.
create_service_for_port(Configuration, OwnerId) ->
    case Configuration of
        #service_port_configuration{ is_public=true } ->
            automate_service_registry:register_public(as_module(Configuration));
        _ ->
            {ok, ServiceId} = automate_service_registry:register_private(as_module(Configuration)),
            ok = automate_service_registry:allow_user(ServiceId, OwnerId),
            {ok, ServiceId}
    end.

as_module(#service_port_configuration{ id=Id
                                     , service_name=Name
                                     }) ->
    #{ name => Name
     , uuid => Id
     , description => <<"A service port configuration">>
     , module => {automate_service_port_engine_service, [Id]}
     }.

-spec set_service_port_configuration(binary(), #service_port_configuration{}, owner_id()) -> {ok, [ request_icon ]}.
set_service_port_configuration(ServicePortId, Configuration=#service_port_configuration{ icon=NewIcon
                                                                                       , is_public=IsPublic
                                                                                       }, OwnerId) ->
    io:fwrite("Setting configuration: ~p~n", [Configuration]),

    Transaction = fun() ->
                          ServiceId = case get_service_id_for_port(ServicePortId) of
                                          {ok, FoundServiceId} ->
                                              ok = automate_service_registry:update_service_module(as_module(Configuration),
                                                                                                   FoundServiceId,
                                                                                                   OwnerId),
                                              ok = automate_service_registry:update_visibility(FoundServiceId, IsPublic),
                                              case IsPublic of
                                                  false ->  %% In case the service is not not public, make sure the owner is allowed
                                                      ok = automate_service_registry:allow_user(FoundServiceId, OwnerId);
                                                  _ -> ok
                                              end,
                                              FoundServiceId;
                                          {error, not_found} ->
                                              {ok, NewServiceId} = create_service_for_port(Configuration, OwnerId),
                                              NewServiceId
                                      end,

                          Previous = mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, ServicePortId),
                          ok = mnesia:write(?SERVICE_PORT_CONFIGURATION_TABLE
                                           , Configuration#service_port_configuration{ service_id=ServiceId }
                                           , write
                                           ),
                          Previous
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Previous} ->
            Todo = case {NewIcon, Previous} of
                       {{ hash, HashType, Hash }, [#service_port_configuration{icon={hash, HashType, Hash}}]} ->
                           %% If it's the same hash, nothing to do
                           [];
                       {{ hash, _HashType, _Hash }, _} ->
                           %% If new is hash, and it's not the same as the old. Request an update
                           [ request_icon ];
                       {_, _} ->
                           %% If neither new nor old are hash, nothing to do
                           []
                   end,
            {ok, Todo};
        {aborted, Reason} ->
            automate_logging:log_platform(error,
                                          io_lib:format("Error saving configuration for bridge id=~p: ~p~n",
                                                        [ServicePortId, Reason])),
            {error, Reason}
    end.

-spec set_notify_signal_listeners([string()], binary()) -> ok.
set_notify_signal_listeners(Content, BridgeId) ->
    {ok, Channels} = list_bridge_channels(BridgeId),
    Pid = self(),
    Node = node(),
    case Content of
        <<"__all__">> ->
            [ automate_channel_engine:monitor_listeners(Channel, Pid, Node) || Channel <- Channels ];
        Keys when is_list(Keys) ->
            %% [ automate_channel_engine:monitor_keys_listeners(Channel, Keys, Pid) || Channel <- Channels ]
            [ automate_channel_engine:monitor_listeners(Channel, Pid, Node) || Channel <- Channels ]
            %% @TODO just listen on specific contant (replace with commented)
    end,
    Entry = #channel_monitor_table_entry{ bridge_id=BridgeId
                                        , pid=Pid
                                        , node=Node
                                        },
    %% Monitor for creation of new channels on bridge
    Transaction = fun() ->
                          ok = mnesia:write(?SERVICE_PORT_CHANNEL_MONITORS_TABLE, Entry, write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec get_signal_listeners([string()], binary()) -> {ok, [{ pid(), binary() | undefined, binary() | undefined}]}.
get_signal_listeners(_Content, BridgeId) ->
    {ok, Channels} = list_bridge_channels(BridgeId),
    {ok, lists:flatmap(fun(Channel) ->
                               {ok, Listeners} = automate_channel_engine:get_listeners_on_channel(Channel),
                               Listeners
                       end, Channels )}.

-spec list_custom_blocks(owner_id()) -> {ok, map()}.
list_custom_blocks(Owner) ->
    Transaction = fun() ->
                          Services = list_userid_ports(Owner) ++ list_public_ports() ++ list_shared_ports(Owner),

                          {ok
                          , maps:from_list(
                              lists:filter(fun (X) -> X =/= none end,
                                           lists:map(fun (PortId) ->
                                                             case is_user_connected_to_bridge(Owner, PortId) of
                                                                 {ok, false} ->
                                                                     none;
                                                                 {ok, true, SharedResources } ->
                                                                     list_blocks_for_port(PortId, SharedResources)
                                                             end
                                                     end,
                                                     Services)))}
                  end,
    automate_storage:wrap_transaction(mnesia:activity(ets, Transaction)).

-spec get_block_definition(BridgeId :: binary(), FunctionId :: binary()) -> {ok, #service_port_block{}}.
get_block_definition(BridgeId, FunctionId) ->
    T = fun() ->
                mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, BridgeId)
        end,
    case mnesia:activity(ets, T) of
        [ #service_port_configuration{ blocks=Blocks } ] ->
            case lists:filter(fun(Block) ->
                                      case Block of
                                          #service_port_block{ block_id=BlockFunId } ->
                                              BlockFunId == FunctionId;
                                          _ ->
                                              false
                                      end
                              end, Blocks) of
                [] -> {error, not_found};
                %% Note that the case for multiple matches is not handled!
                [ Block ] ->
                    {ok, Block}
            end;
        [] ->
            {error, not_found}
    end.


-spec internal_user_id_to_connection_id(owner_id(), binary()) -> {ok, binary()} | {error, not_found} | { error, any() }.
internal_user_id_to_connection_id(Owner, ServicePortId) ->
    case get_all_connections(Owner, ServicePortId) of
        {ok, []} ->
            {error, not_found};
        {ok, [H | _]} ->
            {ok, H}; %% Return first
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_all_connections(owner_id(), binary()) -> {ok, [binary()]} | {error, binary()}.
get_all_connections({OwnerType, OwnerId}, BridgeId) ->
    MatchHead = #user_to_bridge_connection_entry{ id='$1'
                                                , bridge_id='$2'
                                                , owner={'$3', '$4'}
                                                , channel_id='_'
                                                , name='_'
                                                , creation_time='_'
                                                , save_signals='_'
                                                },
    Guards = [ { '==', '$2', BridgeId }
             , { '==', '$3', OwnerType }
             , { '==', '$4', OwnerId }
             ],
    ResultColum = '$1',
    Matcher = [{MatchHead, Guards, [ResultColum]}],

    Transaction = fun() ->
                          {ok, mnesia:select(?USER_TO_BRIDGE_CONNECTION_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec is_user_connected_to_bridge(owner_id(), binary()) -> {ok, false} | {ok, true, all | #{binary() => [binary()] } } | {error, not_found}.
is_user_connected_to_bridge(Owner, BridgeId) ->
    case get_all_connections(Owner, BridgeId) of
        {ok, []} ->
            with_shared_connection(Owner, BridgeId);
        {ok, List} when is_list(List) ->
            {ok, true, all};
        {error, Reason} ->
            {error, Reason}
    end.

-spec connection_id_to_internal_user_id(binary(), binary()) -> {ok, owner_id()} | {error, not_found}.
connection_id_to_internal_user_id(ConnectionId, ServicePortId) ->
    Transaction = fun() ->
                          MatchHead = #user_to_bridge_connection_entry{ id='$1'
                                                                      , bridge_id='$2'
                                                                      , owner='$3'
                                                                      , channel_id='_'
                                                                      , name='_'
                                                                      , creation_time='_'
                                                                      , save_signals='_'
                                                                      },
                          Guards = [ { '==', '$2', ServicePortId }
                                   , { '==', '$1', ConnectionId }
                                   ],
                          ResultColum = '$3',
                          Matcher = [{MatchHead, Guards, [ResultColum]}],

                          mnesia:select(?USER_TO_BRIDGE_CONNECTION_TABLE, Matcher)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, [Result]} ->
            {ok, Result};
        {atomic, []} ->
            {error, not_found};
        %% Not expecting more than one result!
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec get_user_service_ports(owner_id()) -> {ok, [{#service_port_entry{}, #service_port_configuration{}}]}.
get_user_service_ports({OwnerType, OwnerName}) ->
    Transaction = fun() ->
                          MatchHead = #service_port_entry{ id='_'
                                                         , name='_'
                                                         , owner={'$1', '$2'}
                                                         , old_skip_authentication='_'
                                                         },
                          Guards = [ {'==', '$1', OwnerType}
                                   , {'==', '$2', OwnerName}
                                   ],
                          ResultColumn = '$_',
                          Matcher = [{MatchHead, Guards, [ResultColumn]}],



                          {ok, lists:map(fun(Entry=#service_port_entry{ id=Id }) ->
                                                 Configuration = case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, Id) of
                                                     [] -> undefined;
                                                     [Config] -> Config
                                                 end,
                                                 {Entry, Configuration}
                                            end, mnesia:select(?SERVICE_PORT_TABLE, Matcher)) }
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec list_bridge_channels(binary()) -> {ok, [binary()]}.
list_bridge_channels(ServicePortId) ->
    Transaction = fun() ->
                          MatchHead = #service_port_monitor_channel_entry{ id={'_', '$1'}
                                                                         , channel_id='$2'
                                                                         },
                          Guard = {'==', '$1', ServicePortId},
                          ResultColumn = '$2',
                          Matcher = [{MatchHead, [Guard], [ResultColumn]}],

                          {ok, mnesia:select(?SERVICE_PORT_CHANNEL_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec list_bridge_connections(BridgeId :: binary()) -> {ok, [#user_to_bridge_connection_entry{}]} | {error, not_found}.
list_bridge_connections(BridgeId) ->
    MatchHead = #user_to_bridge_connection_entry{ id='_'
                                                , bridge_id='$1'
                                                , owner='_'
                                                , channel_id='_'
                                                , name='_'
                                                , creation_time='_'
                                                , save_signals='_'
                                                },
    Guards = [ { '==', '$1', BridgeId } ],
    ResultColum = '$_',
    Matcher = [{MatchHead, Guards, [ResultColum]}],

    Transaction = fun() ->
                          {ok, mnesia:select(?USER_TO_BRIDGE_CONNECTION_TABLE, Matcher)}
                  end,
    mnesia:activity(ets, Transaction).

-spec list_established_connections(owner_id()) -> {ok, [#user_to_bridge_connection_entry{}]} | {error, not_found}.
list_established_connections({OwnerType, OwnerId}) ->
    MatchHead = #user_to_bridge_connection_entry{ id='_'
                                                , bridge_id='_'
                                                , owner={'$1', '$2'}
                                                , channel_id='_'
                                                , name='_'
                                                , creation_time='_'
                                                , save_signals='_'
                                                },
    Guards = [ { '==', '$1', OwnerType }
             , { '==', '$2', OwnerId }
             ],
    ResultColum = '$_',
    Matcher = [{MatchHead, Guards, [ResultColum]}],

    Transaction = fun() ->
                          {ok, mnesia:select(?USER_TO_BRIDGE_CONNECTION_TABLE, Matcher)}
                  end,
    mnesia:activity(ets, Transaction).

-spec list_established_connections(owner_id(), binary()) -> {ok, [#user_to_bridge_connection_entry{}]} | {error, not_found}.
list_established_connections(Owner, BridgeId) ->
    case list_established_connections(Owner) of
        {ok, Results} ->
            {ok, lists:filter(fun(#user_to_bridge_connection_entry{ bridge_id=ConnBridgeId }) ->
                                      ConnBridgeId == BridgeId
                              end, Results)};
        X ->
            X
    end.


-spec get_connection_owner(binary()) -> {ok, owner_id()} | {error, not_found}.
get_connection_owner(ConnectionId) ->
    T = fun() ->
                case mnesia:read(?USER_TO_BRIDGE_CONNECTION_TABLE, ConnectionId) of
                    [#user_to_bridge_connection_entry{owner=Owner}] ->
                        {ok, Owner};
                    [] ->
                        {error, not_found}
                end
        end,
    automate_storage:wrap_transaction(mnesia:activity(ets, T)).

-spec get_pending_connection_info(binary()) -> {ok, #user_to_bridge_pending_connection_entry{}} | {error, not_found}.
get_pending_connection_info(ConnectionId) ->
    Transaction = fun() ->
                          case mnesia:read(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId) of
                              [] ->
                                  {error, not_found};
                              [ Connection ] ->
                                  {ok, Connection}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, mnesia:error_description(Reason)}
    end.


-spec delete_bridge(owner_id(), binary()) -> ok | {error, binary()}.
delete_bridge(Accessor, BridgeId) ->
    Transaction = fun() ->
                          [#service_port_entry{owner=Owner}] = mnesia:read(?SERVICE_PORT_TABLE, BridgeId),
                          case automate_storage:can_user_edit_as(Accessor, Owner) of
                              true ->
                                  ok = mnesia:delete(?SERVICE_PORT_TABLE, BridgeId, write),
                                  ok = mnesia:delete(?SERVICE_PORT_CONFIGURATION_TABLE, BridgeId, write);
                              %% TODO: remove connection entries
                              false ->
                                  {error, not_authorized}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, mnesia:error_description(Reason)}
    end.

-spec get_or_create_monitor_id(owner_id(), binary()) -> {ok, binary()} | {error, term()}.
get_or_create_monitor_id(Owner, ServicePortId) ->
    Id = {Owner, ServicePortId},
    case mnesia:dirty_read(?SERVICE_PORT_CHANNEL_TABLE, Id) of
        [#service_port_monitor_channel_entry{channel_id=ChannelId}] ->
            {ok, ChannelId};
        [] ->
            {ok, ChannelId} = automate_channel_engine:create_channel(),
            Transaction = fun() ->
                                  ok = mnesia:write(?SERVICE_PORT_CHANNEL_TABLE,
                                                    #service_port_monitor_channel_entry{ id=Id
                                                                                       , channel_id=ChannelId},
                                                    write),

                                  ChannelMonitors = mnesia:read(?SERVICE_PORT_CHANNEL_MONITORS_TABLE, ServicePortId),
                                  {ok, ChannelId, ChannelMonitors}
                          end,
            case mnesia:transaction(Transaction) of
                {atomic, {ok, ChannelId, ChannelMonitors}} ->
                    lists:foreach(fun(#channel_monitor_table_entry{pid=Pid}) ->
                                          Pid ! {automate_service_port_engine, new_channel, {ServicePortId, ChannelId} }
                                  end,
                                  ChannelMonitors),
                    {ok, ChannelId};
                {atomic, Result} ->
                    Result;
                {aborted, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_channel_origin_bridge(binary()) -> {ok, binary()} | {error, not_found}.
get_channel_origin_bridge(ChannelId) ->
    Transaction = fun() ->
                          MatchHead = #service_port_monitor_channel_entry{ id='$1'
                                                                         , channel_id='$2'
                                                                         },
                          Guard = {'==', '$2', ChannelId},
                          ResultColumn = '$1',
                          Matcher = [{MatchHead, [Guard], [ResultColumn]}],

                          mnesia:select(?SERVICE_PORT_CHANNEL_TABLE, Matcher)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, []} ->
            {error, not_found};
        {atomic, [{_UserId, BridgeId}]} ->
            {ok, BridgeId};
        {aborted, Reason} ->
            {error, mnesia:error_description(Reason)}
    end.

-spec set_shared_resource(ConnectionId :: binary(), ResourceName :: binary(), Shares :: map()) -> ok.
set_shared_resource(ConnectionId, ResourceName, Shares) ->
    T = fun() ->
                Existing = mnesia:read(?SERVICE_PORT_SHARED_RESOURCES_TABLE, ConnectionId),
                AboutResource = lists:filter(fun(#bridge_resource_share_entry{resource=Resource}) ->
                                                     Resource == ResourceName
                                             end, Existing),
                ok = lists:foreach(fun(R) ->
                                           ok = mnesia:delete_object(?SERVICE_PORT_SHARED_RESOURCES_TABLE, R, write)
                                   end, AboutResource),
                ok = lists:foreach(fun({ValueId, #{ <<"name">> := ValueName, <<"shared_with">> := Allowed } }) ->
                                           ok = lists:foreach(fun(#{ <<"type">> := OwnerType
                                                                   , <<"id">> := OwnerId
                                                                   }) ->
                                                                      ok = mnesia:write( ?SERVICE_PORT_SHARED_RESOURCES_TABLE
                                                                                       , #bridge_resource_share_entry{ connection_id=ConnectionId
                                                                                                                     , resource=ResourceName
                                                                                                                     , value=ValueId
                                                                                                                     , name=ValueName
                                                                                                                     , shared_with={map_owner_type(OwnerType), OwnerId}
                                                                                                                     }
                                                                                       , write)
                                                              end, Allowed)
                                   end, maps:to_list(Shares))
        end,
    automate_storage:wrap_transaction(mnesia:transaction(T)).

-spec get_connection_shares(ConnectionId :: binary()) -> {ok, #{ binary() => #{ binary() => [ owner_id() ] } } }.
get_connection_shares(ConnectionId) ->
    T = fun() ->
                mnesia:read(?SERVICE_PORT_SHARED_RESOURCES_TABLE, ConnectionId)
        end,
    Permissions = automate_storage:wrap_transaction(mnesia:transaction(T)),
    {ok, shares_list_to_map(Permissions)}.


-spec get_resources_shared_with(Owner :: owner_id()) -> {ok, [#bridge_resource_share_entry{}]}.
get_resources_shared_with(Owner) ->
    T = fun() ->
                mnesia:index_read(?SERVICE_PORT_SHARED_RESOURCES_TABLE, Owner, shared_with)
        end,
    {ok, automate_storage:wrap_transaction(mnesia:activity(ets, T))}.


-spec get_connection_bridge(ConnectionId :: binary()) -> {ok, binary()}.
get_connection_bridge(ConnectionId) ->
    T = fun() ->
                case mnesia:read(?USER_TO_BRIDGE_CONNECTION_TABLE, ConnectionId) of
                    [#user_to_bridge_connection_entry{bridge_id=BridgeId}] ->
                        {ok, BridgeId};
                    [] ->
                        {error, not_found}
                end
        end,
    automate_storage:wrap_transaction(mnesia:activity(ets, T)).


-spec create_bridge_token(BridgeId :: binary(), Owner :: owner_id(), TokenName :: binary(), ExpiresOn :: undefined | non_neg_integer())
                         -> {ok, binary()} | {error, name_taken}.
create_bridge_token(BridgeId, Owner, TokenName, ExpiresOn) ->
    TokenKey = generate_key(),
    CurrentTime = erlang:system_time(second),

    T = fun() ->
                %% Check that there are no tokens with the same name
                BridgeTokens = mnesia:index_read(?BRIDGE_TOKEN_TABLE, BridgeId, bridge_id),
                MatchingTokens = lists:filter(fun(#bridge_token_entry{ token_name=Name}) ->
                                                      TokenName == Name
                                              end, BridgeTokens),
                case MatchingTokens of
                    [] ->
                        ok = mnesia:write(?BRIDGE_TOKEN_TABLE
                                         , #bridge_token_entry{ token_key=TokenKey
                                                              , token_name=TokenName
                                                              , bridge_id=BridgeId
                                                              , creation_time=CurrentTime
                                                              , expiration_time=ExpiresOn
                                                              , last_connection_time=undefined
                                                              }, write),
                        {ok, TokenKey};
                    [#bridge_token_entry{}] ->
                        {error, name_taken}
                end

        end,
    automate_storage:wrap_transaction(mnesia:transaction(T)).

-spec list_bridge_tokens(BridgeId :: binary()) -> {ok, [#bridge_token_entry{}]}.
list_bridge_tokens(BridgeId) ->
    T = fun() ->
                {ok, mnesia:index_read(?BRIDGE_TOKEN_TABLE, BridgeId, bridge_id)}
        end,
    automate_storage:wrap_transaction(mnesia:ets(T)).

-spec delete_bridge_token_by_name(BridgeId :: binary(), TokenName :: binary()) -> ok | {error, not_found}.
delete_bridge_token_by_name(BridgeId, TokenName) ->
    %% TODO: Remove connection from bridges with that token
    T = fun() ->
                BridgeTokens = mnesia:index_read(?BRIDGE_TOKEN_TABLE, BridgeId, bridge_id),
                MatchingTokens = lists:filter(fun(#bridge_token_entry{ token_name=Name}) ->
                                                      TokenName == Name
                                              end, BridgeTokens),
                case MatchingTokens of
                    [] -> {error, not_found};
                    [#bridge_token_entry{ token_key=Key }] ->
                        ok = mnesia:delete(?BRIDGE_TOKEN_TABLE, Key, write)
                end
        end,
    automate_storage:wrap_transaction(mnesia:transaction(T)).

-spec check_bridge_token(BridgeId :: binary(), Token :: binary()) -> {ok, boolean()}.
check_bridge_token(BridgeId, Token) ->
    CurrentTime = erlang:system_time(second),

    T = fun() ->
                case mnesia:read(?BRIDGE_TOKEN_TABLE, Token) of
                    [] -> {ok, false};
                    [TokenRec=#bridge_token_entry{bridge_id=BridgeId}] ->
                        %% TODO: Check that it hasn't expired

                        %% If the bridge could skip auth before, it no longer
                        %% needs it after authenticating correctly.
                        case mnesia:read(?SERVICE_PORT_TABLE, BridgeId) of
                            [#service_port_entry{old_skip_authentication=false}] ->
                                ok; %% Nothing to do
                            [Rec=#service_port_entry{old_skip_authentication=true}] ->
                                ok = mnesia:write(?SERVICE_PORT_TABLE
                                                 , Rec#service_port_entry{old_skip_authentication=false}
                                                 , write)
                        end,

                        %% Update token last used time
                        ok = mnesia:write(?BRIDGE_TOKEN_TABLE, TokenRec#bridge_token_entry{ last_connection_time=CurrentTime }, write),

                        {ok, true}; %% Nothing to do
                    [#bridge_token_entry{bridge_id=_OtherBridgeId}] ->
                        {ok, false}
                end
        end,
    automate_storage:wrap_transaction(mnesia:transaction(T)).

-spec can_skip_authentication(BridgeId :: binary()) -> {ok, boolean()}.
can_skip_authentication(BridgeId) ->
    T = fun() ->
                case mnesia:read(?SERVICE_PORT_TABLE, BridgeId) of
                    [] -> {ok, false};
                    [#service_port_entry{old_skip_authentication=CanSkip}] ->
                        {ok, CanSkip}
                end
        end,
    automate_storage:wrap_transaction(mnesia:ets(T)).

-spec set_save_signals_from_bridge(BridgeId :: binary(), Owner :: owner_id(), SaveSignals :: boolean()) -> ok | {error, _}.
set_save_signals_from_bridge(BridgeId, Owner, SaveSignals) ->
    T = fun() ->
                {ok, Connections} = get_all_connections(Owner, BridgeId),
                lists:foreach(fun(ConnectionId) ->
                                      case mnesia:read(?USER_TO_BRIDGE_CONNECTION_TABLE, ConnectionId) of
                                          [Conn=#user_to_bridge_connection_entry{owner=Owner}] ->
                                              mnesia:write(?USER_TO_BRIDGE_CONNECTION_TABLE
                                                          , Conn#user_to_bridge_connection_entry{save_signals=SaveSignals}
                                                          , write
                                                          )
                                      end
                              end, Connections)
        end,
    automate_storage:wrap_transaction(mnesia:transaction(T)).

-spec check_save_signals_in_connection(ConnectionId :: binary()) -> {ok, false} | {ok, true, {binary(), owner_id()}} | {error, not_found}.
check_save_signals_in_connection(ConnectionId) ->
    T = fun() ->
                case mnesia:read(?USER_TO_BRIDGE_CONNECTION_TABLE, ConnectionId) of
                    [#user_to_bridge_connection_entry{bridge_id=BridgeId, owner=Owner, save_signals=Save}] ->
                        case Save of
                            false ->
                                {ok, false};
                            true ->
                                {ok, true, {BridgeId, Owner}}
                        end;
                    [] ->
                        {error, not_found}
                end
        end,
    automate_storage:wrap_transaction(mnesia:ets(T)).

%%====================================================================
%% Internal functions
%%====================================================================
list_userid_ports({OwnerType, OwnerId}) ->
    MatchHead = #service_port_entry{ id='$1'
                                   , name='_'
                                   , owner={'$2', '$3'}
                                   , old_skip_authentication='_'
                                   },
    Guards = [ {'==', '$2', OwnerType}
             , {'==', '$3', OwnerId}
             ],
    ResultColumn = '$1',
    Matcher = [{MatchHead, Guards, [ResultColumn]}],

    mnesia:select(?SERVICE_PORT_TABLE, Matcher).

list_public_ports() ->
    MatchHead = #service_port_configuration{ id='$1'
                                           , service_name='_'
                                           , service_id='_'
                                           , is_public='$2'
                                           , blocks='_'
                                           , icon='_'
                                           , allow_multiple_connections='_'
                                           , resources='_'
                                           },
    Guard = {'==', '$2', true},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    mnesia:select(?SERVICE_PORT_CONFIGURATION_TABLE, Matcher).

list_shared_ports(Owner) ->
    {ok, Shares} = get_resources_shared_with(Owner),
    lists:map(fun(#bridge_resource_share_entry{ connection_id=ConnectionId }) ->
                      {ok, BridgeId} = automate_service_port_engine:get_connection_bridge(ConnectionId),
                      BridgeId
              end, Shares).

with_shared_connection(Owner, BridgeId) ->
    {ok, Shares} = get_resources_shared_with(Owner),
    SharedResources = lists:foldl(fun(#bridge_resource_share_entry{ connection_id=ConnectionId
                                                                 , resource=Resource
                                                                 , value=Value
                                                                 }, Acc) ->
                                         case automate_service_port_engine:get_connection_bridge(ConnectionId) of
                                             {ok, BridgeId} ->
                                                 case Acc of
                                                     #{ Resource := PrevValues } ->
                                                         Acc#{ Resource => sets:add_element(Value, PrevValues) };
                                                     _ ->
                                                         Acc#{ Resource => sets:from_list([Value]) }
                                                 end;
                                             {ok, _} ->
                                                 Acc
                                         end
                                 end, #{}, Shares),
    case maps:size(SharedResources) > 0 of
        false ->
            {ok, false};
        true ->
            {ok, true, maps:map(fun(_K, V) -> sets:to_list(V) end, SharedResources)}
    end.

list_blocks_for_port(PortId, SharedResources) ->
    case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, PortId) of
        [] -> none;
        [#service_port_configuration{ blocks=Blocks
                                    , service_id=ServiceId
                                    }] ->
            SharedBlocks = case SharedResources of
                               all -> Blocks;
                               Shares when is_map(Shares) ->
                                   lists:filter(fun(Block) ->
                                                        case get_block_resources(Block) of
                                                            %% Shared blocks not refering any resource are a strange case.
                                                            %% For now, avoid relying on them.
                                                            [] -> false;
                                                            Resources ->
                                                                lists:all(fun(BlockResource) ->
                                                                                  maps:is_key(BlockResource, SharedResources)
                                                                          end, Resources)
                                                        end
                                                end, Blocks)
                           end,
            {ServiceId, SharedBlocks}
    end.

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

generate_key() ->
    base64:encode(crypto:strong_rand_bytes(?KEY_RANDOM_LENGTH)).

-spec shares_list_to_map([#bridge_resource_share_entry{}]) -> #{ binary() => #{ binary() => [ owner_id() ] } }.
shares_list_to_map(Permissions) ->
    shares_list_to_map(Permissions, #{}).

shares_list_to_map([], Acc) ->
    maps:map(fun(_K, Values) ->
                     maps:map(fun(_K2, Shares) ->
                                      sets:to_list(Shares)
                              end, Values)
             end, Acc);
shares_list_to_map( [ #bridge_resource_share_entry{ resource=Resource
                                                  , value=Value
                                                  , shared_with=Owner } | T ]
                  , Acc) ->
    WithShare = case Acc of
                    #{ Resource := ResourceVal=#{ Value := Shares } } ->
                        Acc#{ Resource => ResourceVal#{ Value => sets:add_element(Owner, Shares) } };
                    #{ Resource := ResourceVal } ->
                        Acc#{ Resource => ResourceVal#{ Value => sets:from_list([Owner]) } };
                    _ ->
                        Acc#{ Resource => #{ Value => sets:from_list([Owner]) } }
                end,
    shares_list_to_map(T, WithShare).

map_owner_type(Type) when is_binary(Type) ->
    case Type of
        <<"group">> ->
            group;
        <<"user">> ->
            user
    end;
map_owner_type(Type) when is_atom(Type) ->
    Type.


get_block_resources(Block) ->
    Arguments = get_block_arguments(Block),
    Res = lists:filtermap(fun(Arg) ->
                                  case Arg of
                                      #service_port_block_collection_argument{ name=Resource } ->
                                          {true, Resource};
                                      _ ->
                                          false
                                  end
                          end, Arguments ),
    sets:to_list(sets:from_list(Res)).

get_block_arguments(#service_port_block{ arguments=Arguments }) ->
    Arguments;
get_block_arguments(#service_port_trigger_block{ arguments=Arguments }) ->
    Arguments.
