%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_mnesia_backend).

-export([ start_link/0
        , create_service_port/2
        , set_service_port_configuration/3

        , list_custom_blocks/1
        , internal_user_id_to_service_port_user_id/2
        , service_port_user_id_to_internal_user_id/2
        , get_user_service_ports/1
        , list_bridge_channels/1

        , get_bridge_service/2
        , delete_bridge/2

        , get_or_create_monitor_id/2
        , uninstall/0
        , get_channel_origin_bridge/1
        ]).

-include("records.hrl").
-define(SERVICE_PORT_TABLE, automate_service_port_table).
-define(SERVICE_PORT_CONFIGURATION_TABLE, automate_service_port_configuration_table).
-define(SERVICE_PORT_USERID_OBFUSCATION_TABLE, automate_service_port_userid_obfuscation_table).
-define(SERVICE_PORT_CHANNEL_TABLE, automate_service_port_channel_table).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),

    %% Service port identity table
    ok = case mnesia:create_table(?SERVICE_PORT_TABLE,
                                  [ { attributes, record_info(fields, service_port_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Service port configuration table
    ok = case mnesia:create_table(?SERVICE_PORT_CONFIGURATION_TABLE,
                                  [ { attributes, record_info(fields, service_port_configuration)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_configuration }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Service port userId obfuscation
    ok = case mnesia:create_table(?SERVICE_PORT_USERID_OBFUSCATION_TABLE,
                                  [ { attributes, record_info(fields, service_port_user_obfuscation_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_user_obfuscation_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% UserId×ServiceId -> ChannelId
    ok = case mnesia:create_table(?SERVICE_PORT_CHANNEL_TABLE,
                                  [ { attributes, record_info(fields, service_port_monitor_channel_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_monitor_channel_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = mnesia:wait_for_tables([ ?SERVICE_PORT_TABLE
                                , ?SERVICE_PORT_CONFIGURATION_TABLE
                                , ?SERVICE_PORT_USERID_OBFUSCATION_TABLE
                                , ?SERVICE_PORT_CHANNEL_TABLE
                                ], automate_configuration:get_table_wait_time()),
    ignore.


-spec uninstall() -> ok.
uninstall() ->
    {atomic, ok} = mnesia:delete_table(?SERVICE_PORT_TABLE),
    {atomic, ok} = mnesia:delete_table(?SERVICE_PORT_CONFIGURATION_TABLE),
    {atomic, ok} = mnesia:delete_table(?SERVICE_PORT_USERID_OBFUSCATION_TABLE),
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

set_service_port_configuration(ServicePortId, Configuration, OwnerId) ->
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
                          mnesia:write(?SERVICE_PORT_CONFIGURATION_TABLE
                                      , Configuration#service_port_configuration{ service_id=ServiceId }
                                      , write
                                      )
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

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

-spec internal_user_id_to_service_port_user_id(binary(), binary()) -> {ok, binary()}.
internal_user_id_to_service_port_user_id(UserId, ServicePortId) ->
    FullId = {UserId, ServicePortId},
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_PORT_USERID_OBFUSCATION_TABLE, FullId) of
                              [] ->
                                  NewId = generate_id(),
                                  ok = mnesia:write(?SERVICE_PORT_USERID_OBFUSCATION_TABLE,
                                                    #service_port_user_obfuscation_entry{ id=FullId
                                                                                        , obfuscated_id=NewId
                                                                                        }, write),
                                  {ok, NewId};
                              [#service_port_user_obfuscation_entry{ obfuscated_id=ObfuscatedId }] ->
                                  {ok, ObfuscatedId}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec service_port_user_id_to_internal_user_id(binary(), binary()) -> {ok, binary()}.
service_port_user_id_to_internal_user_id(ServicePortUserId, ServicePortId) ->
    Transaction = fun() ->
                          MatchHead = #service_port_user_obfuscation_entry{ id={ '$1', '$2' }
                                                                          , obfuscated_id='$3'
                                                                          },
                          Guards = [ { '==', '$2', ServicePortId }
                                   , { '==', '$3', ServicePortUserId }
                                   ],
                          ResultColum = '$1',
                          Matcher = [{MatchHead, Guards, [ResultColum]}],

                          mnesia:select(?SERVICE_PORT_USERID_OBFUSCATION_TABLE, Matcher)
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

-spec get_bridge_service(binary(), binary()) -> {ok, undefined | binary()}.
get_bridge_service(UserId, BridgeId) ->
    [#service_port_entry{ owner=UserId
                        , service_id=ServiceId
                        }] = mnesia:dirty_read(?SERVICE_PORT_TABLE, BridgeId),
    {ok, ServiceId}.

-spec delete_bridge(binary(), binary()) -> ok | {error, binary()}.
delete_bridge(UserId, BridgeId) ->
    Transaction = fun() ->
                          [#service_port_entry{owner=UserId}] = mnesia:read(?SERVICE_PORT_TABLE, BridgeId),
                          ok = mnesia:delete(?SERVICE_PORT_TABLE, BridgeId, write),
                          ok = mnesia:delete(?SERVICE_PORT_CONFIGURATION_TABLE, BridgeId, write)
                          %% TODO: remove user obfuscation entries
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, mnesia:error_description(Reason)}
    end.

-spec get_or_create_monitor_id(binary(), binary()) -> {ok, binary()}.
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
                                  {ok, ChannelId}
                          end,
            case mnesia:transaction(Transaction) of
                {atomic, Result} ->
                    Result;
                {aborted, Reason} ->
                    {error, Reason, mnesia:error_description(Reason)}
            end
    end.

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
