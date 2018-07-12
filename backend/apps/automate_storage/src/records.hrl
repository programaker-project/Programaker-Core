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

-record(monitor_entry, { id :: binary() | 'none'
                       , user_id :: binary() | 'none'
                       , type :: binary()
                       , name :: binary()
                       , value :: any()
                       }).

-record(stored_program_content, { type
                                , orig
                                , parsed
                                }).

-record(running_program_entry, { program_id
                               , runner_pid
                               , variables
                               , stats
                               }).

-record(existing_service_entry, { id :: binary()
                                , name ::  binary()
                                }).

-record(registered_service_entry, { registration_id :: binary() | ?MNESIA_SELECTOR
                                  , service_id :: binary()      | ?MNESIA_SELECTOR
                                  , user_id :: binary()         | ?MNESIA_SELECTOR
                                  , enabled :: boolean()        | ?MNESIA_SELECTOR
                                  }).

-record(service_registration_token, { token :: binary()         | ?MNESIA_SELECTOR
                                    , service_id :: binary()    | ?MNESIA_SELECTOR
                                    , user_id :: binary()       | ?MNESIA_SELECTOR
                                    }).

-record(telegram_service_registration_entry, { telegram_user_id
                                             , internal_user_id
                                             }).

-record(program_variable_table_entry, { id :: { binary(), binary() } % { program id, variable name }
                                      , value :: any()
                                      }).
