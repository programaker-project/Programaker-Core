-records(database_version_data, { database_name :: atom()
                                , records :: [atom()]
                                , record_name :: atom()
                                , type=set :: set | bag | ordered_set
                                }).

-record(database_version_progression, { base :: #database_version_data{}
                                      , updates :: [#database_version_transformation{}]
                                      }).
