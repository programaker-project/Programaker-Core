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
                      }).

-record(program_metadata, { id
                          , name
                          , link
                          }).

-record(service_metadata, { id
                          , name
                          , link
                          , enabled
                          }).

-record(program_content, { type
                         , orig
                         , parsed
                         }).

-record(service_enable_extra_telegram, { token :: binary()
                                       , bot_name :: binary()
                                       }).

-record(service_enable_how_to, { service :: #service_metadata{}
                               , method :: 'external'
                               , extra :: #service_enable_extra_telegram{}
                               }).
