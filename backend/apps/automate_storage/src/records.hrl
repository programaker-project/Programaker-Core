-include("../../automate_common_types/src/types.hrl").

-type user_status() :: ready | mail_not_verified.
-type time_in_seconds() :: integer().
-type time_in_milliseconds() :: integer().

-record(registered_user_entry, { id :: binary() | ?MNESIA_SELECTOR
                               , username :: binary() | ?MNESIA_SELECTOR
                               , canonical_username :: binary() | ?MNESIA_SELECTOR
                               , password :: binary() | string() | ?MNESIA_SELECTOR
                               , email :: binary() | ?MNESIA_SELECTOR
                               , status :: user_status() | ?MNESIA_SELECTOR
                               , registration_time :: time_in_seconds() | ?MNESIA_SELECTOR

%%% The following entries could be abstracted in a `tags` set entry, but
%%%  that would create a problem when trying to use it on mnesia:select/2 .
                               , is_admin :: boolean() | ?MNESIA_SELECTOR % Platform administration
                               , is_advanced :: boolean() | ?MNESIA_SELECTOR % Advanced features
                               , is_in_preview :: boolean() | ?MNESIA_SELECTOR % Features in beta/preview
                               }).

-type verification_type() :: registration_mail_verification | password_reset_verification.
-record(user_verification_entry, { verification_id :: binary() | ?MNESIA_SELECTOR
                                 , user_id :: binary() | ?MNESIA_SELECTOR
                                 , verification_type :: verification_type() | ?MNESIA_SELECTOR
                                 }).

-record(user_session_entry, { session_id
                            , user_id
                            , session_start_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , session_last_used_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            }).

-record(user_program_entry, { id :: binary()            | ?MNESIA_SELECTOR
                            , user_id ::binary()        | ?MNESIA_SELECTOR
                            , program_name ::binary()   | ?MNESIA_SELECTOR
                            , program_type :: atom()    | ?MNESIA_SELECTOR
                            , program_parsed :: any()   | ?MNESIA_SELECTOR
                            , program_orig :: any()     | ?MNESIA_SELECTOR
                            , enabled=true :: boolean() | ?MNESIA_SELECTOR
                            , program_channel :: binary() | ?MNESIA_SELECTOR
                            , creation_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , last_upload_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , last_successful_call_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , last_failed_call_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            }).

-type log_entry_severity() :: debug | warning | error.
-record(user_program_log_entry, { program_id :: binary()               | ?MNESIA_SELECTOR
                                , thread_id :: binary() | none         | ?MNESIA_SELECTOR
                                , user_id :: binary() | none           | ?MNESIA_SELECTOR
                                , block_id :: binary() | undefined     | ?MNESIA_SELECTOR
                                , event_data :: _                      | ?MNESIA_SELECTOR
                                , event_message :: binary()            | ?MNESIA_SELECTOR
                                , event_time :: time_in_milliseconds() | ?MNESIA_SELECTOR
                                , severity :: log_entry_severity()     | ?MNESIA_SELECTOR
                                , exception_data :: none | {_, _, _}   | ?MNESIA_SELECTOR
                                }).

-record(user_generated_log_entry, { program_id :: binary()               | ?MNESIA_SELECTOR
                                  , block_id :: binary() | undefined     | ?MNESIA_SELECTOR
                                  , severity :: log_entry_severity()     | ?MNESIA_SELECTOR
                                  , event_time :: time_in_milliseconds() | ?MNESIA_SELECTOR
                                  , event_message :: binary()            | ?MNESIA_SELECTOR
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

-record(running_program_entry, { program_id :: program_id()          | ?MNESIA_SELECTOR
                               , runner_pid :: running_program_pid() | ?MNESIA_SELECTOR
                               , variables  :: any()                 | ?MNESIA_SELECTOR
                               , stats      :: any()                 | ?MNESIA_SELECTOR
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

-record(custom_signal_entry, { id    :: binary() | ?MNESIA_SELECTOR
                             , name  :: binary() | ?MNESIA_SELECTOR
                             , owner :: binary() | ?MNESIA_SELECTOR %% User id
                             }).


-record(storage_configuration_entry, { id :: any()
                                     , value :: any()
                                     }).
