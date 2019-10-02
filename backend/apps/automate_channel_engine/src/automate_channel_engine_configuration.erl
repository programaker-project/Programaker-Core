%%%-------------------------------------------------------------------
%% @doc automate channel engine configuration and versioning
%% @end
%%%-------------------------------------------------------------------
-module(automate_channel_engine_configuration).

-export([ get_versioning/1
        ]).

-include("databases.hrl").
-include("../../automate_storage/src/versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(_Nodes) ->
    %% Live channels table
    Version_1 = [ #database_version_data{ database_name=?LIVE_CHANNELS_TABLE
                                        , records=[ live_channel_id, stats ]
                                        , record_name=live_channels_table_entry
                                        }
                ],

    #database_version_progression
        { base=Version_1
        , updates=[]
        }.
