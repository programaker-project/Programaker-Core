%%%-------------------------------------------------------------------
%% @doc automate storage configuration and versioning.
%% @end
%%%-------------------------------------------------------------------

-module(automate_storage_configuration).

-export([ get_versioning/1
        ]).

-include("./databases.hrl").
-include("./versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(Nodes) ->
    %% Registered users table
    Version_1 = [ #database_version_data{ database_name=?REGISTERED_USERS_TABLE
                                        , records=[ id, username, password, email ]
                                        , record_name=registered_user_entry
                                        }

                  %% User session table
                , #database_version_data{ database_name=?USER_SESSIONS_TABLE
                                        , records=[ session_id, user_id, session_start_time ]
                                        , record_name=user_session_entry
                                        }

                  %% User monitors table
                , #database_version_data{ database_name=?USER_MONITORS_TABLE
                                        , records=[ id, user_id, type, name, value ]
                                        , record_name=monitor_entry
                                        }

                  %% User programs table
                , #database_version_data{ database_name=?USER_PROGRAMS_TABLE
                                        , records=[ id, user_id, program_name, program_type
                                                  , program_parsed, program_orig
                                                  ]
                                        , record_name=user_program_entry
                                        }

                  %% Running program threads table
                , #database_version_data{ database_name=?RUNNING_THREADS_TABLE
                                        , records=[ thread_id
                                                  , runner_pid
                                                  , parent_program_id
                                                  , instructions
                                                  , memory
                                                  , instruction_memory
                                                  , position
                                                  , stats
                                                  ]
                                        , record_name=running_program_thread_entry
                                        }

                  %% Program tags table
                , #database_version_data{ database_name=?PROGRAM_TAGS_TABLE
                                        , records=[ program_id, tags ]
                                        , record_name=program_tags_entry
                                        }

                  %% Running programs table
                , #database_version_data{ database_name=?RUNNING_PROGRAMS_TABLE
                                        , records=[ program_id, runner_pid, variables, stats ]
                                        , record_name=running_program_entry
                                        }

                  %% Program variable table
                , #database_version_data{ database_name=?PROGRAM_VARIABLE_TABLE
                                        , records=[ id, value ]
                                        , record_name=program_variable_table_entry
                                        }
                ],

    #database_version_progression
        { base=Version_1
        , updates=
              [ #database_version_transformation
                %% Add *Installation configuration table*
                %%
                %% Keeps track of the database versions and offers a place
                %% to store Key-Value data.
                %%
                { id=1
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?INSTALLATION_CONFIGURATION_TABLE
                                       , records=[ id
                                                 , value
                                                 ]
                                       , record_name=storage_configuration_entry
                                       }, Nodes),

                                ok = mnesia:wait_for_tables([ ?INSTALLATION_CONFIGURATION_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add *enabled* property to user programs
                %%
                %% Allows for disabling programs while keeping it's threads
                %%  and variables untouched.
              , #database_version_transformation
                { id=2
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_PROGRAMS_TABLE,
                                                 fun({user_program_entry, Id, UserId, ProgramName,
                                                      ProgramType, ProgramParsed, ProgramOrig }) ->
                                                         %% Replicate the entry. Just set enabled to true.
                                                         {user_program_entry, Id, UserId, ProgramName,
                                                          ProgramType, ProgramParsed, ProgramOrig, true }
                                                 end,
                                                 [ id, user_id, program_name, program_type, program_parsed, program_orig, enabled],
                                                 user_program_entry
                                                )
                        end
                }

                %% Add *custom signals* table
                %%
                %% Allows for user-defined (not bridge-defined) signals.
              , #database_version_transformation
                { id=3
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?CUSTOM_SIGNALS_TABLE
                                       , records=[ id
                                                 , name
                                                 , owner
                                                 ]
                                       , record_name=custom_signal_entry
                                       }, Nodes),

                                ok = mnesia:wait_for_tables([ ?CUSTOM_SIGNALS_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add *status* to user table.
                %%
                %% If a user "comes" from an earlier version the status is 'ready'.
              , #database_version_transformation
                { id=4
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?REGISTERED_USERS_TABLE,
                                                 fun({registered_user_entry, Id, Username, Password, Email }) ->
                                                         %% Replicate the entry. Set status to ready.
                                                         {registered_user_entry, Id, Username, Password, Email,
                                                          ready }
                                                 end,
                                                 [ id, username, password, email, status ],
                                                 registered_user_entry
                                                )
                        end
                }

                %% Create user verification table.
              , #database_version_transformation
                { id=5
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?USER_VERIFICATION_TABLE
                                       , records=[ id
                                                 , user_id
                                                 , verification_type
                                                 ]
                                       , record_name=user_verification_entry
                                       }, Nodes),

                                ok = mnesia:wait_for_tables([ ?USER_VERIFICATION_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% - Add *registration_time* to user table.
                %%
                %% Previous records are set to `0`.
                %% This is to avoid a fake spike on registered users on the migration date.
              , #database_version_transformation
                { id=6
                , apply=fun() ->
                                mnesia:transform_table(
                                  ?REGISTERED_USERS_TABLE,
                                  fun({registered_user_entry, Id, Username, Password, Email, Status }) ->
                                          %% Replicate the entry. Set registration time to unknown.
                                          {registered_user_entry, Id, Username, Password, Email,
                                           Status, 0 }
                                  end,
                                  [ id, username, password, email, status, registration_time ],
                                  registered_user_entry
                                 )
                        end
                }

                %% - Add *session_last_used_time* to sessions table.
                %%
                %% Previous records are set to `0`.
                %% This is to avoid a fake spike on the migration date.
              , #database_version_transformation
                { id=7
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_SESSIONS_TABLE,
                                                 fun({user_session_entry, SessionId, UserId, SessionStartTime }) ->
                                                         %% Replicate the entry. Set session_last_used_time to unknown.
                                                         {user_session_entry, SessionId, UserId, SessionStartTime,
                                                          0 }
                                                 end,
                                                 [ session_id, user_id, session_start_time, session_last_used_time ],
                                                 user_session_entry
                                                )
                        end
                }

                %% Add user program logs table.
              , #database_version_transformation
                { id=8
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?USER_PROGRAM_LOGS_TABLE
                                       , records=[ program_id
                                                 , thread_id
                                                 , user_id
                                                 , block_id
                                                 , event_data
                                                 , event_message
                                                 , event_time
                                                 , severity
                                                 , exception_data
                                                 ]
                                       , record_name=user_program_log_entry
                                       , type=bag
                                       }, Nodes),

                                ok = mnesia:wait_for_tables([ ?USER_PROGRAM_LOGS_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add `update_channel` entry to programs table.
                %%
                %% This is used to stream the changes happening on the programs.
                %% The channel will be deleted when the program is.
              , #database_version_transformation
                { id=9
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 automate_user_programs, %% ?USER_PROGRAMS_TABLE
                                                 fun({user_program_entry, Id, UserId, ProgramName,
                                                      ProgramType, ProgramParsed, ProgramOrig, Enabled }) ->
                                                         %% Replicate the entry. Just create an empty program channel.

                                                         { user_program_entry, Id, UserId, ProgramName,
                                                           ProgramType, ProgramParsed, ProgramOrig, Enabled,
                                                           undefined }
                                                 end,
                                                 [ id, user_id, program_name, program_type, program_parsed, program_orig, enabled
                                                 , program_channel
                                                 ],
                                                 user_program_entry
                                                ),

                                %% After the table is updated, generate the new channels
                                %% This apparently cannot be done inside the mnesia:transform_table.
                                ok = db_map(automate_user_programs, %% ?USER_PROGRAMS_TABLE
                                       fun({user_program_entry, Id, UserId, ProgramName,
                                            ProgramType, ProgramParsed, ProgramOrig, Enabled, ProgramChannel }) ->
                                               NewChannel = case ProgramChannel of
                                                                undefined ->
                                                                    {ok, CreatedChannel} = automate_channel_engine:create_channel(),
                                                                    io:fwrite("Created channel: ~p~n", [CreatedChannel]),
                                                                    CreatedChannel;
                                                                _ ->
                                                                    ProgramChannel
                                                            end,
                                               {user_program_entry, Id, UserId, ProgramName,
                                                ProgramType, ProgramParsed, ProgramOrig, Enabled, NewChannel }
                                       end)
                        end
                , revert=fun() ->
                                %% Before the table is updated, remove the old channels
                                 %% This apparently cannot be done inside the mnesia:transform_table.
                                 ok = db_map(automate_user_programs, %% ?USER_PROGRAMS_TABLE
                                             fun({user_program_entry, Id, UserId, ProgramName,
                                                  ProgramType, ProgramParsed, ProgramOrig, Enabled, ProgramChannel }) ->
                                                     case ProgramChannel of
                                                         undefined ->
                                                             ok;
                                                         _ ->
                                                             %% Replicate the entry. Just create a program channel.

                                                             Result = automate_channel_engine:delete_channel(ProgramChannel),
                                                             io:fwrite("Deleting channel ~p: ~p~n", [ProgramChannel, Result])
                                                     end,
                                                     { user_program_entry, Id, UserId, ProgramName,
                                                       ProgramType, ProgramParsed, ProgramOrig, Enabled }
                                             end),

                                 {atomic, ok} = mnesia:transform_table(
                                                  automate_user_programs, %% ?USER_PROGRAMS_TABLE
                                                  fun({user_program_entry, Id, UserId, ProgramName
                                                      , ProgramType, ProgramParsed, ProgramOrig, Enabled
                                                      , _ProgramChannel }) ->
                                                          { user_program_entry, Id, UserId, ProgramName,
                                                            ProgramType, ProgramParsed, ProgramOrig, Enabled }
                                                  end,
                                                  [ id, user_id, program_name, program_type, program_parsed, program_orig, enabled
                                                  ],
                                                  user_program_entry
                                                 )
                         end
                }

                %% Add stability metrics to programs table.
                %%
                %% - creation_time : To put the rest in context
                %% - last_upload_time : To know how "fresh" the program is
                %% - last_successful_call_time : Relative to the next metric puts a number on the current health of the program
                %% - last_failed_call_time : Relative to the previous metric puts a number on the current health of the program
                %%
              , #database_version_transformation
                { id=10
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 automate_user_programs, %% ?USER_PROGRAMS_TABLE
                                                 fun({user_program_entry, Id, UserId, ProgramName,
                                                      ProgramType, ProgramParsed, ProgramOrig,
                                                      Enabled, ProgramChannel }) ->
                                                         %% Replicate the entry. Fill unknown times with '0'

                                                         { user_program_entry, Id, UserId, ProgramName
                                                         , ProgramType, ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                         , 0, 0, 0, 0
                                                         }

                                                 end,
                                                 [ id, user_id, program_name, program_type, program_parsed, program_orig, enabled, program_channel
                                                 , creation_time, last_upload_time, last_successful_call_time, last_failed_call_time
                                                 ],
                                                 user_program_entry
                                                )
                        end
                }

                %% - Add user tags `is_admin`, `is_advanced`, `is_in_preview` to user table.
                %%
                %% Previous records are set to `false`.
              , #database_version_transformation
                { id=11
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?REGISTERED_USERS_TABLE,
                                                 fun({registered_user_entry, Id, Username, Password, Email, Status, RegistrationTime }) ->
                                                         %% Replicate the entry. Set is_admin/advanced/in_preview to false.
                                                         { registered_user_entry, Id, Username, Password, Email, Status, RegistrationTime
                                                         , false, false, false
                                                         }
                                                 end,
                                                 [ id, username, password, email, status, registration_time
                                                 , is_admin, is_advanced, is_in_preview
                                                 ],
                                                 registered_user_entry
                                                )
                        end
                }

                %% - Add `canonical_username` to user table.
                %%
                %% Previous records are set to lowercased usernames, but they might not be correct usernames.
              , #database_version_transformation
                { id=12
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?REGISTERED_USERS_TABLE,
                                                 fun({registered_user_entry, Id, Username, Password
                                                     , Email, Status, RegistrationTime
                                                     , IsAdmin, IsAdvanced, IsInPreview
                                                     }) ->
                                                         CanonicalUsername = automate_storage_utils:canonicalize(Username),

                                                         %% Replicate the entry. Set canonicalized username.
                                                         { registered_user_entry, Id, Username, CanonicalUsername, Password
                                                         , Email, Status, RegistrationTime
                                                         , IsAdmin, IsAdvanced, IsInPreview
                                                         }
                                                 end,
                                                 [ id, username, canonical_username, password, email, status, registration_time
                                                 , is_admin, is_advanced, is_in_preview
                                                 ],
                                                 registered_user_entry
                                                )
                        end
                }

                %% Add user generated logs table.
              , #database_version_transformation
                { id=13
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                  #database_version_data
                                  { database_name=?USER_GENERATED_LOGS_TABLE
                                  , records=[ program_id
                                            , block_id
                                            , severity
                                            , event_time
                                            , event_message
                                            ]
                                  , record_name=user_generated_log_entry
                                  , type=bag
                                  }, Nodes),

                                ok = mnesia:wait_for_tables([ ?USER_GENERATED_LOGS_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add user editor events table.
              , #database_version_transformation
                { id=14
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?USER_PROGRAM_EVENTS_TABLE
                                       , records=[ program_id
                                                 , event
                                                 , event_tag
                                                 ]
                                       , record_name=user_program_editor_event
                                       , type=bag
                                       }, Nodes),

                                ok = mnesia:wait_for_tables([ ?USER_PROGRAM_EVENTS_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add program checkpoints table.
              , #database_version_transformation
                { id=15
                , apply=fun() ->
                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?USER_PROGRAM_CHECKPOINTS_TABLE
                                       , records=[ program_id
                                                 , user_id
                                                 , event_time
                                                 , content
                                                 ]
                                       , record_name=user_program_checkpoint
                                       , type=bag
                                       }, Nodes),

                                ok = mnesia:wait_for_tables([ ?USER_PROGRAM_CHECKPOINTS_TABLE ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Introduce user groups
              , #database_version_transformation
                { id=16
                , apply=fun() ->
                                %% This table might be problematic, so force to load it here
                                ok = automate_storage_maintenance:wait_table(?USER_PROGRAM_LOGS_TABLE),

                                ok = automate_storage_versioning:create_database(
                                       #database_version_data
                                       { database_name=?USER_GROUPS_TABLE
                                       , records=[ id
                                                 , name
                                                 ]
                                       , record_name=user_group_entry
                                       , type=set
                                       }, Nodes),

                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_PROGRAM_LOGS_TABLE,
                                                 fun({ user_program_log_entry
                                                     , ProgramId, ThreadId, UserId, BlockId, EventData, EventMessage
                                                     , EventTime, Severity, ExceptionData
                                                     }) ->
                                                         { user_program_log_entry
                                                         , ProgramId, ThreadId, {user, UserId}, BlockId, EventData, EventMessage
                                                         , EventTime, Severity, ExceptionData
                                                         }
                                                 end,
                                                 [ program_id, thread_id, owner, block_id, event_data
                                                 , event_message, event_time, severity, exception_data
                                                 ],
                                                 user_program_log_entry
                                                ),

                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_PROGRAMS_TABLE,
                                                 fun({ user_program_entry
                                                     , Id, UserId, ProgramName, ProgramType
                                                     , ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                     , CreationTime, LastUploadTime, LastSuccessfulCalltime
                                                     , LastFailedCallTime
                                                     }) ->
                                                         { user_program_entry
                                                         , Id, {user, UserId}, ProgramName, ProgramType
                                                         , ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                         , CreationTime, LastUploadTime, LastSuccessfulCalltime
                                                         , LastFailedCallTime
                                                         }
                                                 end,
                                                 [ id, owner, program_name, program_type, program_parsed, program_orig, enabled, program_channel
                                                 , creation_time, last_upload_time, last_successful_call_time, last_failed_call_time
                                                 ],
                                                 user_program_entry
                                                ),

                                {atomic, ok} = mnesia:transform_table(
                                                 ?CUSTOM_SIGNALS_TABLE,
                                                 fun({ custom_signal_entry
                                                     , Id, Name, Owner
                                                     }) ->
                                                         { custom_signal_entry
                                                         , Id, Name, {user, Owner}
                                                         }
                                                 end,
                                                 [ id, name, owner
                                                 ],
                                                 custom_signal_entry
                                                ),

                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_MONITORS_TABLE,
                                                 fun({ monitor_entry
                                                     , Id, UserId, Type, Name, Value
                                                     }) ->
                                                         { monitor_entry
                                                         , Id, {user, UserId}, Type, Name, Value
                                                         }
                                                 end,
                                                 [ id, owner, type, name, value
                                                 ],
                                                 monitor_entry
                                                ),

                                ok = mnesia:wait_for_tables([ ?USER_GROUPS_TABLE, ?USER_PROGRAM_LOGS_TABLE
                                                            , ?USER_PROGRAMS_TABLE, ?CUSTOM_SIGNALS_TABLE, ?USER_MONITORS_TABLE
                                                            ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add user groups permissions
              , #database_version_transformation
                { id=17
                , apply=fun() ->

                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_GROUPS_TABLE,
                                                 fun( {user_group_entry, Id, Name} ) ->
                                                         { user_group_entry, Id, Name, automate_storage_utils:canonicalize(Name), false }
                                                 end,
                                                 [ id, name, canonical_name, public ],
                                                 user_group_entry),

                                {atomic, ok} = mnesia:add_table_index(?USER_GROUPS_TABLE, canonical_name),
                                {atomic, ok} = mnesia:add_table_index(?USER_PROGRAMS_TABLE, owner),
                                {atomic, ok} = mnesia:add_table_index(?USER_MONITORS_TABLE, owner),

                                {atomic, ok} = mnesia:create_table(?USER_GROUP_PERMISSIONS_TABLE,
                                                                   [ { attributes, [group_id, user_id, role] }
                                                                   , { disc_copies, Nodes }
                                                                   , { record_name, user_group_permissions_entry }
                                                                   , { type, bag }
                                                                   , { index, [ user_id ]  }
                                                                   ]),

                                ok = mnesia:wait_for_tables([ ?USER_GROUP_PERMISSIONS_TABLE
                                                            ],
                                                            automate_configuration:get_table_wait_time())
                        end
                }

                %% Add groups creation time
              , #database_version_transformation
                { id=18
                , apply=fun() ->

                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_GROUPS_TABLE,
                                                 fun( {user_group_entry, Id, Name, CanonicalName, Public} ) ->
                                                         {user_group_entry, Id, Name, CanonicalName, Public, 0}
                                                 end,
                                                 [ id, name, canonical_name, public, creation_time ],
                                                 user_group_entry
                                                )
                        end
                }

                %% Add groups creation time
              , #database_version_transformation
                { id=19
                , apply=fun() ->

                                {atomic, ok} = mnesia:create_table(?PROGRAM_PAGES_TABLE,
                                                                   [ { attributes, [ page_id, program_id, contents ] }
                                                                   , { disc_copies, Nodes }
                                                                   , { record_name, program_pages_entry }
                                                                   , { type, set }
                                                                   , { index, [ program_id ] }
                                                                   ]),

                                    ok = mnesia:wait_for_tables([ ?PROGRAM_PAGES_TABLE
                                                                ],
                                                                automate_configuration:get_table_wait_time())

                        end
                }

                %% Add widget's last value table
              , #database_version_transformation
                { id=20
                , apply=fun() ->

                                {atomic, ok} = mnesia:create_table(?PROGRAM_WIDGET_VALUE_TABLE,
                                                                   [ { attributes, [ widget_id, program_id, value ] }
                                                                   , { disc_copies, Nodes }
                                                                   , { record_name, program_widget_value_entry }
                                                                   , { type, set }
                                                                   , { index, [ program_id ] }
                                                                   ]),

                                    ok = mnesia:wait_for_tables([ ?PROGRAM_WIDGET_VALUE_TABLE
                                                                ],
                                                                automate_configuration:get_table_wait_time())

                        end
                }

                %% Add asset table time and re-structure the asset directories
              , #database_version_transformation
                { id=21
                , apply=fun() ->

                                {atomic, ok} = mnesia:create_table(?USER_ASSET_TABLE,
                                                                   [ { attributes, [ asset_id, owner_id, mime_type ] }
                                                                   , { disc_copies, Nodes }
                                                                   , { record_name, user_asset_entry }
                                                                   , { type, set }
                                                                   , { index, [ owner_id ] }
                                                                   ]),

                                ok = mnesia:wait_for_tables([ ?USER_ASSET_TABLE
                                                            ],
                                                            automate_configuration:get_table_wait_time()),

                                %% Update user/group picture paths
                                {atomic, ok} = mnesia:transaction(
                                                 fun() ->
                                                         Migrate = fun(Tab, AssetDirectory) ->
                                                                           G = mnesia:all_keys(Tab),
                                                                           io:fwrite("~p ~p~n", [length(G), AssetDirectory]),

                                                                           GetOldPath = fun(Id) ->
                                                                                                list_to_binary([ automate_configuration:asset_directory(list_to_binary(["public/", AssetDirectory, "/"]))
                                                                                                               , "/", Id
                                                                                                               ])
                                                                                        end,

                                                                           HasPicture = (
                                                                             fun(Id) ->
                                                                                     Path = GetOldPath(Id),
                                                                                     filelib:is_regular(Path)
                                                                             end),

                                                                           WithPicture = lists:filter(HasPicture, G),
                                                                           io:fwrite("~p ~p with old picture~n", [length(WithPicture), AssetDirectory]),

                                                                           ok = lists:foreach(
                                                                                  fun(Id) ->
                                                                                          io:fwrite("Updating ~p~n", [Id]),
                                                                                          OldPath = GetOldPath(Id),
                                                                                          TmpPath = list_to_binary([OldPath, ".tmp"]),
                                                                                          NewPath = list_to_binary([OldPath, "/picture"]),

                                                                                          ok = file:rename(OldPath, TmpPath),
                                                                                          ok = filelib:ensure_dir(NewPath),
                                                                                          ok = file:rename(TmpPath, NewPath)

                                                                                  end, WithPicture)
                                                                   end,
                                                         ok = Migrate(?REGISTERED_USERS_TABLE, "users"),
                                                         ok = Migrate(?USER_GROUPS_TABLE, "groups")
                                                 end)
                        end
                }

                %% Add `creation_time` and `used` fields to user verification entries.
                %%
                %% This should allow to keep it for longer to avoid showing errors when applied twice.
              , #database_version_transformation
                { id=22
                , apply=fun() ->
                                CurrentTime = erlang:system_time(second),

                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_VERIFICATION_TABLE,
                                                 fun({user_verification_entry, Id, UserId, VerificationType }) ->
                                                         %% Replicate the entry. Set Creation and used to { current_time(), false }.
                                                         { user_verification_entry, Id, UserId, VerificationType
                                                         , CurrentTime, false }
                                                 end,
                                                 [ id, user_id, verification_type, creation_time, used ],
                                                 user_verification_entry
                                                )
                        end
                }

                %% Add visibility to user programs
              , #database_version_transformation
                { id=23
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_PROGRAMS_TABLE,
                                                 fun({ user_program_entry
                                                     , Id, UserId, ProgramName, ProgramType
                                                     , ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                     , CreationTime, LastUploadTime, LastSuccessfulCalltime
                                                     , LastFailedCallTime
                                                     }) ->
                                                         { user_program_entry
                                                         , Id, UserId, ProgramName, ProgramType
                                                         , ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                         , CreationTime, LastUploadTime, LastSuccessfulCalltime
                                                         , LastFailedCallTime
                                                         , false %% Default to non-public
                                                         }
                                                 end,
                                                 [ id, owner, program_name, program_type, program_parsed, program_orig, enabled, program_channel
                                                 , creation_time, last_upload_time, last_successful_call_time, last_failed_call_time
                                                 , is_public
                                                 ],
                                                 user_program_entry
                                                )
                        end
                }

                %% Add profile configuration
              , #database_version_transformation
                { id=24
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_PROGRAMS_TABLE,
                                                 fun({ user_program_entry
                                                     , Id, UserId, ProgramName, ProgramType
                                                     , ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                     , CreationTime, LastUploadTime, LastSuccessfulCalltime
                                                     , LastFailedCallTime, IsPublic
                                                     }) ->
                                                         %% Translate `is_public` to `visibility`
                                                         Visibility = case IsPublic of
                                                                          true -> shareable;
                                                                          false -> private
                                                                      end,

                                                         { user_program_entry
                                                         , Id, UserId, ProgramName, ProgramType
                                                         , ProgramParsed, ProgramOrig, Enabled, ProgramChannel
                                                         , CreationTime, LastUploadTime, LastSuccessfulCalltime
                                                         , LastFailedCallTime
                                                         , Visibility
                                                         }
                                                 end,
                                                 [ id, owner, program_name, program_type, program_parsed, program_orig, enabled, program_channel
                                                 , creation_time, last_upload_time, last_successful_call_time, last_failed_call_time
                                                 , visibility
                                                 ],
                                                 user_program_entry
                                                ),

                                    {atomic, ok} = mnesia:create_table(?USER_PROFILE_LISTINGS_TABLE,
                                                                       [ { attributes, [ id, groups ] }
                                                                       , { disc_copies, Nodes }
                                                                       , { record_name, user_profile_listings_entry }
                                                                       , { type, set }
                                                                       , { index, [ ] }
                                                                       ]),

                                ok = mnesia:wait_for_tables([ ?USER_PROFILE_LISTINGS_TABLE
                                                            ],
                                                            automate_configuration:get_table_wait_time())
                       end
                }

                %% Add direction to program threads
              , #database_version_transformation
                { id=25
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?RUNNING_THREADS_TABLE,
                                                 fun({ running_program_thread_entry
                                                     , ThreadId, RunnerPid, ParentProgramId
                                                     , Instructions, Memory, InstructionMemory
                                                     , Position, Stats
                                                     }) ->
                                                         { running_program_thread_entry
                                                         , ThreadId, RunnerPid, ParentProgramId
                                                         , Instructions, Memory, InstructionMemory
                                                         , Position, Stats
                                                         , forward
                                                         }
                                                 end,
                                                 [ thread_id, runner_pid, parent_program_id
                                                 , instructions, memory, instruction_memory
                                                 , position, stats, direction
                                                 ],
                                                 running_program_thread_entry
                                                )
                       end
                }

                %% Add `min.level to privately use bridge` to group entry.
                %% Default to `not_allowed`.
              , #database_version_transformation
                { id=26
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?USER_GROUPS_TABLE,
                                                 fun( {user_group_entry, Id, Name, CanonicalName, Public, CreationTime} ) ->
                                                         {user_group_entry, Id, Name, CanonicalName, Public, CreationTime, not_allowed}
                                                 end,
                                                 [ id, name, canonical_name, public, creation_time, min_level_for_private_bridge_usage ],
                                                 user_group_entry
                                                )
                        end
                }

                %% Index program variables by ProgramId.
              , #database_version_transformation
                { id=27
                , apply=fun() ->

                                ok = mnesia:wait_for_tables([ ?PROGRAM_VARIABLE_TABLE
                                                            ],
                                                            automate_configuration:get_table_wait_time()),

                                    {atomic, ok} = mnesia:transform_table(
                                                     ?PROGRAM_VARIABLE_TABLE,
                                                     fun( {program_variable_table_entry, {ProgramId, VarName}, Value} ) ->
                                                             {program_variable_table_entry, {ProgramId, VarName}, ProgramId, Value}
                                                     end,
                                                     [ id, program_id, value ],
                                                     program_variable_table_entry
                                                    ),

                                {atomic, ok} = mnesia:add_table_index(?PROGRAM_VARIABLE_TABLE, program_id)
                        end
                }
              ]
        }.

db_map(Database, Function) ->
    Transaction = fun() ->
                          ok = mnesia:write_lock_table(Database),
                          ok = db_map_iter(Database, Function, mnesia:first(Database))
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            io:fwrite("[Storage/Migration] Error on migration: ~p~n", [Reason]),
            {error, Reason}
    end.

db_map_iter(_Database, _Function, '$end_of_table') ->
    ok;
db_map_iter(Database, Function, Key) ->
    [Element] = mnesia:read(Database, Key),
    NewElement = Function(Element),
    %% Note that the old element is not removed. An update cannot change the ID.
    ok = mnesia:write(Database, NewElement, write),
    db_map_iter(Database, Function, mnesia:next(Database, Key)).
