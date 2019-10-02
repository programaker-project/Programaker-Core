-module(automate_service_port_engine_configuration).

%%%-------------------------------------------------------------------
%% @doc automate service port engine configuration and versioning
%% @end
%%%-------------------------------------------------------------------

-export([ get_versioning/1
        ]).

-include("databases.hrl").
-include("../../automate_storage/src/versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(_Nodes) ->
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

                  %% Service port userId obfuscation
                , #database_version_data{ database_name=?SERVICE_PORT_USERID_OBFUSCATION_TABLE
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
        , updates=[]
        }.
