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
                                automate_storage_versioning:create_database(
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
                                mnesia:transform_table(
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
                                automate_storage_versioning:create_database(
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
                                mnesia:transform_table(
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
                                automate_storage_versioning:create_database(
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
                                          %% Replicate the entry. Set status to ready.
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
                                mnesia:transform_table(
                                  ?USER_SESSIONS_TABLE,
                                  fun({user_session_entry, SessionId, UserId, SessionStartTime }) ->
                                          %% Replicate the entry. Set status to ready.
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
                                automate_storage_versioning:create_database(
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
                                mnesia:transform_table(
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
                                db_map(automate_user_programs, %% ?USER_PROGRAMS_TABLE
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
                                db_map(automate_user_programs, %% ?USER_PROGRAMS_TABLE
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

                                 mnesia:transform_table(
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
                                mnesia:transform_table(
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
