%%%-------------------------------------------------------------------
%% @doc automate service port engine configuration and versioning
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_configuration).

-export([ get_versioning/1
        ]).

-include("databases.hrl").
-include("../../automate_storage/src/versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(Nodes) ->
    %% Service port identity table
    Version_1 = [ #database_version_data{ database_name=?SERVICE_PORT_TABLE
                                        , records=[ id, name, owner, service_id ]
                                        , record_name=service_port_entry
                                        }

                  %% Service port configuration table
                , #database_version_data{ database_name=?SERVICE_PORT_CONFIGURATION_TABLE
                                        , records=[ id, service_name, service_id, is_public, blocks ]
                                        , record_name=service_port_configuration
                                        }

                  %% Service port userId obfuscation (deprecated)
                , #database_version_data{ database_name=automate_service_port_userid_obfuscation_table
                                        , records=[ id, obfuscated_id ]
                                        , record_name=service_port_user_obfuscation_entry
                                        }

                  %% UserId×ServiceId -> ChannelId
                , #database_version_data{ database_name=?SERVICE_PORT_CHANNEL_TABLE
                                        , records=[ id, channel_id ]
                                        , record_name=service_port_monitor_channel_entry
                                        }
                ],

    #database_version_progression
        { base=Version_1
        , updates=[ #database_version_transformation
                    %% 1. Add *User -> Bridge connection* table
                    %%
                    %%    Keeps track of the bridges a user has authenticated himself into.
                    %%
                    %% 2. Delete the UserId-obfuscation table.
                    %%
                    %%    This is now managed in the connections table.
                    %%
                    %% 3. Create a temporary "connection establishment" table
                    %%
                    %%    This helps keep track of the ongoing registrations, for
                    %%    processes that use side-channels, like chats.
                    { id=1
                    , apply=fun() ->
                                    ok = automate_storage_versioning:create_database(
                                      #database_version_data
                                      { database_name=?USER_TO_BRIDGE_CONNECTION_TABLE
                                      , records=[ id
                                                , bridge_id
                                                , user_id
                                                , channel_id
                                                , name
                                                , creation_time
                                                ]
                                      , record_name=user_to_bridge_connection_entry
                                      }, Nodes),

                                    ok = automate_storage_versioning:create_database(
                                      #database_version_data
                                      { database_name=?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE
                                      , records=[ id
                                                , bridge_id
                                                , user_id
                                                , channel_id
                                                , creation_time
                                                ]
                                      , record_name=user_to_bridge_pending_connection_entry
                                      }, Nodes),

                                    ok = mnesia:wait_for_tables([ ?USER_TO_BRIDGE_CONNECTION_TABLE
                                                                , ?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE
                                                                ],
                                                                automate_configuration:get_table_wait_time()),

                                    MigrateConnections =
                                        fun() ->
                                                Conversion =
                                                    fun({service_port_user_obfuscation_entry, {UserId, BridgeId}, ObfuscatedId}) ->
                                                            {ok, ChannelId} = automate_channel_engine:create_channel(),
                                                            { user_to_bridge_connection_entry
                                                            , ObfuscatedId
                                                            , BridgeId
                                                            , UserId
                                                            , ChannelId
                                                            , undefined
                                                            , 0
                                                            }
                                                    end,
                                                ok = db_map_table_to_table(automate_service_port_userid_obfuscation_table,
                                                                      ?USER_TO_BRIDGE_CONNECTION_TABLE,
                                                                      Conversion)
                                        end,
                                    {atomic, ok} = mnesia:transaction(MigrateConnections),

                                    {atomic, ok} = mnesia:delete_table(automate_service_port_userid_obfuscation_table)
                            end
                    }

                  , #database_version_transformation
                    %% Add *icons* to service_configuration
                    { id=2
                    , apply=fun() ->
                                    {atomic, ok} = mnesia:transform_table(
                                                     ?SERVICE_PORT_CONFIGURATION_TABLE,
                                                     fun({service_port_configuration, Id, ServiceName, ServiceId,
                                                          IsPublic, Blocks }) ->
                                                             %% Replicate the entry. Just set 'icon' to undefined.
                                                             {service_port_configuration, Id, ServiceName, ServiceId,
                                                              IsPublic, Blocks, undefined }
                                                     end,
                                                     [ id, service_name, service_id, is_public, blocks, icon ],
                                                     service_port_configuration
                                                    )
                            end
                    }

                  , #database_version_transformation
                    %% Add *allow_multiple_connection* to service_configuration table
                    { id=3
                    , apply=fun() ->
                                    {atomic, ok} = mnesia:transform_table(
                                                     ?SERVICE_PORT_CONFIGURATION_TABLE,
                                                     fun({service_port_configuration, Id, ServiceName, ServiceId,
                                                          IsPublic, Blocks, Icon }) ->
                                                             %% Replicate the entry. Just set 'allow_multiple_connections' to false.
                                                             {service_port_configuration, Id, ServiceName, ServiceId,
                                                              IsPublic, Blocks, Icon, false }
                                                     end,
                                                     [ id, service_name, service_id, is_public, blocks, icon, allow_multiple_connections ],
                                                     service_port_configuration
                                                    )
                            end
                    }

                  , #database_version_transformation
                    %% Introduce user groups
                    { id=3
                    , apply=fun() ->
                                    {atomic, ok} = mnesia:transform_table(
                                                     ?SERVICE_PORT_TABLE,
                                                     fun({service_port_entry
                                                         , Id, Name, Owner, ServiceId
                                                         }) ->
                                                             {service_port_entry
                                                             , Id, Name, {user, Owner}, ServiceId
                                                             }
                                                     end,
                                                     [ id, name, owner, service_id ],
                                                     service_port_entry
                                                    ),

                                    {atomic, ok} = mnesia:transform_table(
                                                     ?USER_TO_BRIDGE_CONNECTION_TABLE,
                                                     fun({user_to_bridge_connection_entry
                                                         , Id, BridgeId, UserId, ChannelId, Name, CreationTime
                                                         }) ->
                                                             {user_to_bridge_connection_entry
                                                             , Id, BridgeId, {user, UserId}, ChannelId, Name, CreationTime
                                                             }
                                                     end,
                                                     [ id, bridge_id, owner, channel_id, name, creation_time ],
                                                     user_to_bridge_connection_entry
                                                    ),

                                    {atomic, ok} = mnesia:transform_table(
                                                     ?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE,
                                                     fun({user_to_bridge_pending_connection_entry
                                                         , Id, BridgeId, UserId, ChannelId, CreationTime
                                                         }) ->
                                                             {user_to_bridge_pending_connection_entry
                                                             , Id, BridgeId, {user, UserId}, ChannelId, CreationTime
                                                             }
                                                     end,
                                                     [ id, bridge_id, owner, channel_id, creation_time ],
                                                     user_to_bridge_pending_connection_entry
                                                    ),

                                    ok = mnesia:wait_for_tables([ ?SERVICE_PORT_TABLE, ?USER_TO_BRIDGE_CONNECTION_TABLE
                                                                , ?USER_TO_BRIDGE_PENDING_CONNECTION_TABLE
                                                                ],
                                                                automate_configuration:get_table_wait_time())
                            end
                    }
                  ]
        }.


db_map_table_to_table(FromTable, ToTable, Function) ->
    Transaction = fun() ->
                          ok = mnesia:write_lock_table(FromTable),
                          ok = db_map_iter_transfer(FromTable, ToTable, Function, mnesia:first(FromTable))
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            io:fwrite("[Storage/Migration] Error on migration: ~p~n", [Reason]),
            {error, Reason}
    end.

db_map_iter_transfer(_FromTable, _ToTable, _Function, '$end_of_table') ->
    ok;
db_map_iter_transfer(FromTable, ToTable, Function, Key) ->
    [Element] = mnesia:read(FromTable, Key),
    NewElement = Function(Element),

    ok = mnesia:write(ToTable, NewElement, write),
    db_map_iter_transfer(FromTable, ToTable, Function, mnesia:next(FromTable, Key)).
