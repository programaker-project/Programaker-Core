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

-record(registered_service_entry, { registration_id :: binary()
                                  , service_id :: binary()
                                  , user_id :: binary()
                                  , enabled :: boolean()
                                  }).

-record(service_registration_token, { token :: binary()
                                    , service_id :: binary()
                                    , user_id :: binary()
                                    }).

-record(telegram_service_registration_entry, { telegram_user_id
                                             , internal_user_id
                                             }).
