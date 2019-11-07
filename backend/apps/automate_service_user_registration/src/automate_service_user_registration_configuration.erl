%%%-------------------------------------------------------------------
%% @doc automate service user configuration's configuration and versioning
%% @end
%%%-------------------------------------------------------------------
-module(automate_service_user_registration_configuration).

-export([ get_versioning/1
        ]).

-include("databases.hrl").
-include("../../automate_storage/src/versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(_Nodes) ->
    %% Service registration token table
    Version_1 = [ #database_version_data{ database_name=?SERVICE_REGISTRATION_TOKEN_TABLE
                                        , records=[ token, service_id, user_id ]
                                        , record_name=service_registration_token
                                        }
                ],

    #database_version_progression
        { base=Version_1
        , updates=[]
        }.
