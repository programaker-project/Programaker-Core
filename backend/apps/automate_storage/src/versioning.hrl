-record(database_version_data, { database_name :: atom()
                               , records :: [atom()]
                               , record_name :: atom()
                               , type=set :: set | bag | ordered_set
                               }).

-type database_version_transformation_id() :: pos_integer().
-record(database_version_transformation, { id :: database_version_transformation_id()
                                         , apply :: function()
                                         }).

-record(database_version_progression, { base :: [#database_version_data{}]
                                      , updates :: [#database_version_transformation{}]
                                      }).
