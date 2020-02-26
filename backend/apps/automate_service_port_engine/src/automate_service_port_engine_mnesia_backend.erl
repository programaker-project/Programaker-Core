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
        , internal_user_id_to_connection_id/2
        , is_user_connected_to_bridge/2
        , connection_id_to_internal_user_id/2
        , get_user_service_ports/1
        , list_bridge_channels/1
        , list_established_connections/1
        , get_pending_connection_info/1

        , gen_pending_connection/2
        , establish_connection/4
        , establish_connection/3

        , get_service_id_for_port/1
        , get_bridge_info/1
        , get_all_bridge_info/1
        , delete_bridge/2

        , get_or_create_monitor_id/2
        , uninstall/0
        , get_channel_origin_bridge/1
        ]).

-include("records.hrl").
-include("databases.hrl").

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


-spec create_service_port(binary(), binary()) -> {ok, binary()} | {error, _, string()}.
create_service_port(UserId, ServicePortName) ->
    ServicePortId = generate_id(),
    Entry = #service_port_entry{ id=ServicePortId
                               , name=ServicePortName
                               , owner=UserId
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

-spec gen_pending_connection(binary(), binary()) -> {ok, binary()}.
gen_pending_connection(BridgeId, UserId) ->
    ConnectionId = generate_id(),
    CurrentTime = erlang:system_time(second),

    Transaction = fun() ->
                          {ok, ChannelId} = automate_channel_engine:create_channel(),
                          Entry = #user_to_bridge_pending_connection_entry{ id=ConnectionId
                                                                          , bridge_id=BridgeId
                                                                          , user_id=UserId
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
-spec establish_connection(binary(), binary(), binary(), binary()) -> ok | {error, not_found}.
establish_connection(BridgeId, UserId, ConnectionId, Name) ->
    CurrentTime = erlang:system_time(second),
    Transaction = fun() ->
                          case mnesia:read(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId) of
                              [] ->
                                  {error, not_found};
                              [ #user_to_bridge_pending_connection_entry{ bridge_id=BridgeId
                                                                        , user_id=UserId
                                                                        , channel_id=ChannelId
                                                                        } ] ->
                                  ok = mnesia:delete(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId, write),

                                  Entry = #user_to_bridge_connection_entry{ id=ConnectionId
                                                                          , bridge_id=BridgeId
                                                                          , user_id=UserId
                                                                          , channel_id=ChannelId
                                                                          , name=Name
                                                                          , creation_time=CurrentTime
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
                                                                        , user_id=UserId
                                                                        , channel_id=ChannelId
                                                                        } ] ->
                                  ok = mnesia:delete(?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE, ConnectionId, write),

                                  Entry = #user_to_bridge_connection_entry{ id=ConnectionId
                                                                          , bridge_id=BridgeId
                                                                          , user_id=UserId
                                                                          , channel_id=ChannelId
                                                                          , name=Name
                                                                          , creation_time=CurrentTime
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


-spec get_bridge_info(binary()) -> {ok, #service_port_metadata{}}.
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

-spec set_service_port_configuration(binary(), #service_port_configuration{}, binary()) -> {ok, [ request_icon ]}.
set_service_port_configuration(ServicePortId, Configuration=#service_port_configuration{ icon=NewIcon }, OwnerId) ->
    io:fwrite("Setting configuration: ~p~n", [Configuration]),

    ServiceId = case get_service_id_for_port(ServicePortId) of
                    {ok, FoundServiceId} ->
                        ok = automate_service_registry:update_service_module(as_module(Configuration),
                                                                             FoundServiceId,
                                                                             OwnerId),
                        FoundServiceId;
                    {error, not_found} ->
                        {ok, NewServiceId} = create_service_for_port(Configuration, OwnerId),
                        NewServiceId
                end,

    Transaction = fun() ->
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

-spec list_custom_blocks(binary()) -> {ok, map()}.
list_custom_blocks(UserId) ->
    Transaction = fun() ->
                          Services = list_userid_ports(UserId) ++ list_public_ports(),
                          {ok
                          , maps:from_list(
                              lists:filter(fun (X) -> X =/= none end,
                                           lists:map(fun (PortId) ->
                                                             list_blocks_for_port(PortId)
                                                     end,
                                                     Services)))}
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec internal_user_id_to_connection_id(binary(), binary()) -> {ok, binary()}.
internal_user_id_to_connection_id(UserId, ServicePortId) ->
    case get_all_connections(UserId, ServicePortId) of
        {ok, []} ->
            {error, not_found};
        {ok, [H | _]} ->
            {ok, H}; %% Return first
        {error, Reason} ->
            {error, Reason}
    end.

get_all_connections(UserId, BridgeId) ->
    MatchHead = #user_to_bridge_connection_entry{ id='$1'
                                                , bridge_id='$2'
                                                , user_id='$3'
                                                , channel_id='_'
                                                , name='_'
                                                , creation_time='_'
                                                },
    Guards = [ { '==', '$2', BridgeId }
             , { '==', '$3', UserId }
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
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec is_user_connected_to_bridge(binary(), binary()) -> {ok, boolean()} | {error, not_found}.
is_user_connected_to_bridge(UserId, BridgeId) ->
    case get_all_connections(UserId, BridgeId) of
        {ok, []} ->
            {ok, false};
        {ok, List} when is_list(List) ->
            {ok, true};
        {error, Reason} ->
            {error, Reason}
    end.

-spec connection_id_to_internal_user_id(binary(), binary()) -> {ok, binary()} | {error, not_found}.
connection_id_to_internal_user_id(ConnectionId, ServicePortId) ->
    Transaction = fun() ->
                          MatchHead = #user_to_bridge_connection_entry{ id='$1'
                                                                      , bridge_id='$2'
                                                                      , user_id='$3'
                                                                      , channel_id='_'
                                                                      , name='_'
                                                                      , creation_time='_'
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

-spec get_user_service_ports(binary()) -> {ok, [map()]}.
get_user_service_ports(UserId) ->
    Transaction = fun() ->
                          MatchHead = #service_port_entry{ id='_'
                                                         , name='_'
                                                         , owner='$1'
                                                         , service_id='_'
                                                         },
                          Guard = {'==', '$1', UserId},
                          ResultColumn = '$_',
                          Matcher = [{MatchHead, [Guard], [ResultColumn]}],

                          {ok, mnesia:select(?SERVICE_PORT_TABLE, Matcher)}
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

-spec list_established_connections(binary()) -> {ok, [#user_to_bridge_connection_entry{}]}.
list_established_connections(UserId) ->
    MatchHead = #user_to_bridge_connection_entry{ id='_'
                                                , bridge_id='_'
                                                , user_id='$1'
                                                , channel_id='_'
                                                , name='_'
                                                , creation_time='_'
                                                },
    Guards = [ { '==', '$1', UserId } ],
    ResultColum = '$_',
    Matcher = [{MatchHead, Guards, [ResultColum]}],

    Transaction = fun() ->
                          {ok, mnesia:select(?USER_TO_BRIDGE_CONNECTION_TABLE, Matcher)}
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec get_pending_connection_info(binary()) -> {ok, #user_to_bridge_pending_connection_entry{}}.
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


-spec delete_bridge(binary(), binary()) -> ok | {error, binary()}.
delete_bridge(UserId, BridgeId) ->
    Transaction = fun() ->
                          [#service_port_entry{owner=UserId}] = mnesia:read(?SERVICE_PORT_TABLE, BridgeId),
                          ok = mnesia:delete(?SERVICE_PORT_TABLE, BridgeId, write),
                          ok = mnesia:delete(?SERVICE_PORT_CONFIGURATION_TABLE, BridgeId, write)
                          %% TODO: remove connection entries
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, mnesia:error_description(Reason)}
    end.

-spec get_or_create_monitor_id(binary(), binary()) -> {ok, binary()} | {error, term(), binary()}.
get_or_create_monitor_id(UserId, ServicePortId) ->
    Id = {UserId, ServicePortId},
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
                    {error, Reason, mnesia:error_description(Reason)}
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

%%====================================================================
%% Internal functions
%%====================================================================
list_userid_ports(UserId) ->
    MatchHead = #service_port_entry{ id='$1'
                                   , name='_'
                                   , owner='$2'
                                   , service_id='_'
                                   },
    Guard = {'==', '$2', UserId},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    mnesia:select(?SERVICE_PORT_TABLE, Matcher).

list_public_ports() ->
    MatchHead = #service_port_configuration{ id='$1'
                                           , service_name='_'
                                           , service_id='_'
                                           , is_public='$2'
                                           , blocks='_'
                                           , icon='_'
                                           , allow_multiple_connections='_'
                                           },
    Guard = {'==', '$2', true},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    mnesia:select(?SERVICE_PORT_CONFIGURATION_TABLE, Matcher).

list_blocks_for_port(PortId) ->
    case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, PortId) of
        [] -> none;
        [#service_port_configuration{ blocks=Blocks
                                    , service_id=ServiceId
                                    }] ->
            {ServiceId, Blocks}
    end.

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
