-define(DEFAULT_PROGRAM_TYPE, scratch_program).

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

-record(user_program, { id :: binary()
                      , owner :: #{ type => (user | group), id => binary() }
                      , program_name :: binary()
                      , program_type :: binary() | atom()
                      , program_parsed :: any()
                      , program_orig :: any()
                      , enabled :: boolean()
                      , last_upload_time :: integer()
                      }).

-record(program_metadata, { id
                          , name
                          , link
                          , enabled
                          , type
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
