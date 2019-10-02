-record(rest_session,
        { user_id
        , session_id
        }).

-record(registration_rec,
        { email
        , password
        , username
        }).

-record(login_rec,
        { password
        , username
        }).

-record(user_program, { id
                      , user_id
                      , program_name
                      , program_type
                      , program_parsed
                      , program_orig
                      , enabled
                      }).

-record(program_metadata, { id
                          , name
                          , link
                          , enabled
                          }).

-record(program_content, { type
                         , orig
                         , parsed
                         }).

-record(monitor_metadata, { id
                          , name
                          , link
                          }).

-record(monitor_descriptor, { type :: binary()
                            , name :: binary()
                            , value :: any()
                            }).

-record(service_metadata, { id
                          , name
                          , link
                          , enabled
                          }).
