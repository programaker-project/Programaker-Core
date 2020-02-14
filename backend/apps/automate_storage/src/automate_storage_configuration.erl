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

                %% Add *status* to user table.
                %%
                %% If a user "comes" from an earlier version the status is 'ready'.
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
              ]
        }.
