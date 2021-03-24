-include("../../automate_common_types/src/types.hrl").
-ifndef(AUTOMATE_STORAGE_RECORDS).
-define(AUTOMATE_STORAGE_RECORDS, true).

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

-record(user_profile_listings_entry, { id :: owner_id()
                                     , groups :: [binary()]
                                     }).

-type user_in_group_role() :: admin | editor | viewer.
-type group_metadata_edition() :: #{ public => boolean(), min_level_for_private_bridge_usage => user_in_group_role() | not_allowed }.

-record(user_group_entry, { id :: binary() | ?MNESIA_SELECTOR
                          , name :: binary() | ?MNESIA_SELECTOR
                          , canonical_name :: binary() | ?MNESIA_SELECTOR
                          , public :: boolean() | ?MNESIA_SELECTOR
                          , creation_time :: time_in_seconds() | ?MNESIA_SELECTOR
                          , min_level_for_private_bridge_usage :: user_in_group_role() | not_allowed | ?MNESIA_SELECTOR
                          }).

-record(user_group_permissions_entry, { group_id :: binary() | ?MNESIA_SELECTOR
                                      , user_id :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                                      , role  :: user_in_group_role()
                                      }).

-type verification_type() :: registration_mail_verification | password_reset_verification.

-record(user_verification_entry, { verification_id :: binary() | ?MNESIA_SELECTOR
                                 , user_id :: binary() | ?MNESIA_SELECTOR
                                 , verification_type :: verification_type() | ?MNESIA_SELECTOR
                                 , creation_time :: time_in_seconds() | ?MNESIA_SELECTOR
                                 , used :: boolean() | ?MNESIA_SELECTOR
                                 }).

-type session_expiration_time() :: session | time_in_seconds() | never.
-type session_scope_item() :: check %% Any permission is enough for check
                            | ui    %% For ui-related functions only. Not really related to permissions actually...
                            | list_custom_blocks %% Might be merged into read_program

                              %% Connections
                            | { edit_connection, binary() }
                            | {read_how_to_enable_service, binary()}
                            | list_connections_available | list_connections_established

                              %% Assets
                            | list_assets | create_assets

                              %% Programs
                            | list_programs | create_programs | {delete_program, binary()}
                            | {read_program, binary()} | {edit_program, binary()}
                            | {edit_program_status, binary()} | {read_program_logs, binary()} | {edit_program_metadata, binary()}
                            | {render_program, binary()}

                              %% Bridges
                            | list_bridges | create_bridges
                            | call_any_bridge | {call_bridge, binary(), binary()}
                            | { call_bridge_callback, binary() }
                            | {delete_bridge, binary()} | {delete_bridge_tokens, binary()}

                              %% Bridge signals
                            | { read_bridge_signal, binary() } %% Moderately sensitive
                            | { read_bridge_signal, binary(), binary() } %% More specific

                              %% Bridge shares
                            | { list_bridge_resources, binary() }
                            | { edit_connection_shares, binary() }

                              %% Services
                            | list_services | create_services

                              %% Groups
                            | list_groups | create_groups

                              %% Templates
                            | list_templates | create_templates
                            | {read_template, binary()} | { edit_template, binary() } | { delete_template, binary() }

                              %% Custom signals
                            | list_custom_signals | create_custom_signals

                              %% Monitors
                            | list_monitors | create_monitors

                              %% Private, but don't affect mechanics
                            | edit_user_profile
                            | edit_user_picture
                            | edit_user_settings

                              %% Actions on groups
                            | {list_group_bridges, binary()}
                            | {create_group_bridges, binary()}
                            | {list_group_programs, binary()} | { create_group_programs, binary() }
                            | {read_group_info, binary()} | {list_group_connections_available, binary()}| {list_group_connections_established, binary()}
                            | {edit_group_picture, binary()}
                            | {list_group_shares, binary()}

                              %% Actions on programs
                            | { list_program_shares, binary() }
                            | { call_program_bridge_callback, binary(), binary() }
                            | { list_program_connections_available, binary() } | { list_program_connections_established, binary() }
                            | { list_program_services, binary() }
                            | { read_program_variables, binary() } | { edit_program_variables, binary() }
                            | { list_program_monitors, binary() }

                            | { establish_program_connection, binary() } % This naming goes out
                                                % of list/read/edit/delete/create/call convention.
                                                %
                                                % Ideally we should remove it



                              %% Especially sensitive
                            | { admin_group_info, binary() }
                            | { list_bridge_tokens, binary() } | { create_bridge_tokens, binary() }
                            | create_api_tokens

                              %% Admin-only
                            | admin_read_stats
                            | admin_list_users
                              .
-type session_scope() :: all | [session_scope_item()].
-record(user_session_entry, { session_id
                            , user_id
                            , session_start_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , session_last_used_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , session_scope :: session_scope() | ?MNESIA_SELECTOR
                            , session_expiration_time :: session_expiration_time() | ?MNESIA_SELECTOR
                            }).

-record(user_program_entry, { id :: binary()            | ?MNESIA_SELECTOR
                            , owner :: owner_id()       | ?OWNER_ID_MNESIA_SELECTOR
                            , program_name :: binary()  | ?MNESIA_SELECTOR
                            , program_type :: atom()    | ?MNESIA_SELECTOR
                            , program_parsed :: any()   | ?MNESIA_SELECTOR
                            , program_orig :: any()     | ?MNESIA_SELECTOR
                            , enabled=true :: boolean() | ?MNESIA_SELECTOR
                            , program_channel :: binary() | ?MNESIA_SELECTOR
                            , creation_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , last_upload_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , last_successful_call_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , last_failed_call_time :: time_in_seconds() | ?MNESIA_SELECTOR
                            , visibility :: user_program_visibility() | ?MNESIA_SELECTOR
                            }).

-type log_severity() :: debug | info | warning | error.
-record(user_program_log_entry, { program_id :: binary()               | ?MNESIA_SELECTOR
                                , thread_id :: binary() | none         | ?MNESIA_SELECTOR
                                , owner :: owner_id() | none           | ?OWNER_ID_MNESIA_SELECTOR
                                , block_id :: binary() | undefined     | ?MNESIA_SELECTOR
                                , event_data :: _                      | ?MNESIA_SELECTOR
                                , event_message :: binary()            | ?MNESIA_SELECTOR
                                , event_time :: time_in_milliseconds() | ?MNESIA_SELECTOR
                                , severity :: log_severity()           | ?MNESIA_SELECTOR
                                , exception_data :: none | {_, _, _}   | ?MNESIA_SELECTOR
                                }).

-record(user_program_editor_event, { program_id :: binary()
                                   , event :: any()
                                   , event_tag :: { integer(), integer() }
                                   }).

-record(user_program_checkpoint, { program_id :: binary()
                                 , user_id :: binary()
                                 , event_time :: time_in_milliseconds()
                                 , content :: any()
                                 }).

-record(user_generated_log_entry, { program_id :: binary()               | ?MNESIA_SELECTOR
                                  , block_id :: binary() | undefined     | ?MNESIA_SELECTOR
                                  , severity :: log_severity()           | ?MNESIA_SELECTOR
                                  , event_time :: time_in_milliseconds() | ?MNESIA_SELECTOR
                                  , event_message :: binary()            | ?MNESIA_SELECTOR
                                 }).

-record(program_tags_entry, { program_id
                            , tags
                            }).

-record(editable_user_program_metadata, { program_name :: binary()
                                        }).

-record(monitor_entry, { id :: binary() | 'none'      | ?MNESIA_SELECTOR
                       , owner :: owner_id() | 'none' | ?OWNER_ID_MNESIA_SELECTOR
                       , type :: binary()             | ?MNESIA_SELECTOR
                       , name :: binary()             | ?MNESIA_SELECTOR
                       , value :: any()               | ?MNESIA_SELECTOR
                       }).

-record(stored_program_content, { type
                                , orig
                                , parsed
                                , pages :: map()
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
                                      , direction :: thread_direction() | ?MNESIA_SELECTOR
                                      }).

-record(registered_service_entry, { registration_id :: binary() | ?MNESIA_SELECTOR
                                  , service_id :: binary()      | ?MNESIA_SELECTOR
                                  , user_id :: binary()         | ?MNESIA_SELECTOR
                                  , enabled :: boolean()        | ?MNESIA_SELECTOR
                                  }).

-record(program_variable_table_entry, { id :: { binary(), binary() } % { program id, variable name }
                                      , program_id :: binary()
                                      , value :: any()
                                      }).

-record(custom_signal_entry, { id    :: binary()   | ?MNESIA_SELECTOR
                             , name  :: binary()   | ?MNESIA_SELECTOR
                             , owner :: owner_id() | ?OWNER_ID_MNESIA_SELECTOR
                             }).


-record(storage_configuration_entry, { id :: any()
                                     , value :: any()
                                     }).

-record(program_pages_entry, { page_id :: {binary(), binary()} %% {ProgramId, PagePath}
                             , program_id :: binary() %% Used for indexing on program-wide operations
                             , contents :: any() %% Type to be more strictly defined?
                                           %% TODO: Access permissions?
                             }).

-record(program_widget_value_entry, { widget_id :: {binary(), binary()} %% {ProgramId, <<widget_type, "." , widget_id>>}
                                    , program_id :: binary() %% Used for indexing on program-wide operations
                                    , value :: any() %% Type to be more strictly defined?
                                    }).

-type mime_type() :: { binary(), binary() | undefined }. %% { Type, Subtype }
-record(user_asset_entry, { asset_id :: { owner_id(), binary() } %% { OwnerId, AssetId }
                          , owner_id :: owner_id() %% For listing
                          , mime_type :: mime_type()
                          }).

-endif.
