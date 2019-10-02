-include("../../automate_common_types/src/types.hrl").

-record(registered_user_entry, { id
                               , username
                               , password
                               , email
                               }).

-record(user_session_entry, { session_id
                            , user_id
                            , session_start_time
                            }).

-record(user_program_entry, { id :: binary()            | ?MNESIA_SELECTOR
                            , user_id ::binary()        | ?MNESIA_SELECTOR
                            , program_name ::binary()   | ?MNESIA_SELECTOR
                            , program_type :: atom()    | ?MNESIA_SELECTOR
                            , program_parsed :: any()   | ?MNESIA_SELECTOR
                            , program_orig :: any()     | ?MNESIA_SELECTOR
                            , enabled=true :: boolean() | ?MNESIA_SELECTOR
                            }).

-record(program_tags_entry, { program_id
                            , tags
                            }).

-record(editable_user_program_metadata, { program_name :: binary()
                                        }).

-record(monitor_entry, { id :: binary() | 'none'      | ?MNESIA_SELECTOR
                       , user_id :: binary() | 'none' | ?MNESIA_SELECTOR
                       , type :: binary()             | ?MNESIA_SELECTOR
                       , name :: binary()             | ?MNESIA_SELECTOR
                       , value :: any()               | ?MNESIA_SELECTOR
                       }).

-record(stored_program_content, { type
                                , orig
                                , parsed
                                }).

-type program_id() :: binary().
-type thread_id() :: binary().
-type running_program_pid() :: pid().

-record(running_program_entry, { program_id :: program_id()
                               , runner_pid :: running_program_pid()
                               , variables
                               , stats
                               }).

-record(running_program_thread_entry, { thread_id :: thread_id()        | ?MNESIA_SELECTOR
                                      , runner_pid :: undefined | pid() | ?MNESIA_SELECTOR
                                      , parent_program_id :: program_id() | ?MNESIA_SELECTOR
                                      , instructions :: [_]| ?MNESIA_SELECTOR  %% The parent program instructions might have changed
                                      , memory :: map()    | ?MNESIA_SELECTOR
                                      , instruction_memory :: map() | ?MNESIA_SELECTOR
                                      , position :: [pos_integer()] | ?MNESIA_SELECTOR
                                      , stats :: any() | ?MNESIA_SELECTOR
                                      }).

-record(registered_service_entry, { registration_id :: binary() | ?MNESIA_SELECTOR
                                  , service_id :: binary()      | ?MNESIA_SELECTOR
                                  , user_id :: binary()         | ?MNESIA_SELECTOR
                                  , enabled :: boolean()        | ?MNESIA_SELECTOR
                                  }).

-record(program_variable_table_entry, { id :: { binary(), binary() } % { program id, variable name }
                                      , value :: any()
                                      }).


-record(storage_configuration_entry, { id :: any()
                                     , value :: any()
                                     }).
