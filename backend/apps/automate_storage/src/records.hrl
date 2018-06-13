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
