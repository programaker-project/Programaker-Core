%%%-------------------------------------------------------------------
%% @doc automate service registry configuration and versioning
%% @end
%%%-------------------------------------------------------------------
-module(automate_service_registry_configuration).

-export([ get_versioning/1
        ]).

-include("databases.hrl").
-include("../../automate_storage/src/versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(_Nodes) ->
    Version_1 = [ #database_version_data{ database_name=?SERVICE_REGISTRY_TABLE
                                        , records=[ id, public, name, description, module ]
                                        , record_name=services_table_entry
                                        }

                , #database_version_data{ database_name=?USER_SERVICE_ALLOWANCE_TABLE
                                        , records=[ service_id, user_id ]
                                        , record_name=user_service_allowance_entry
                                        , type=bag
                                        }

                , #database_version_data{ database_name=?SERVICE_CONFIGURATION_TABLE
                                        , records=[ configuration_id, value ]
                                        , record_name=services_configuration_entry
                                        }
                ],

    #database_version_progression
        { base=Version_1
        , updates=[]
        }.
