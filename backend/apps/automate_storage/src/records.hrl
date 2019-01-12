-define(MNESIA_SELECTOR, '_' | '$1'| '$2'| '$3'| '$4').

-record(registered_user_entry, { id
                               , username
                               , password
                               , email
                               }).

-record(user_session_entry, { session_id
                            , user_id
                            , session_start_time
                            }).

-record(user_program_entry, { id
                            , user_id
                            , program_name
                            , program_type
                            , program_parsed
                            , program_orig
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
-type running_program_pid() :: pid().

-record(running_program_entry, { program_id :: program_id()
                               , runner_pid :: running_program_pid()
                               , variables
                               , stats
                               }).

-record(registered_service_entry, { registration_id :: binary() | ?MNESIA_SELECTOR
                                  , service_id :: binary()      | ?MNESIA_SELECTOR
                                  , user_id :: binary()         | ?MNESIA_SELECTOR
                                  , enabled :: boolean()        | ?MNESIA_SELECTOR
                                  }).

-record(program_variable_table_entry, { id :: { binary(), binary() } % { program id, variable name }
                                      , value :: any()
                                      }).


