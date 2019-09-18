%%%-------------------------------------------------------------------
%% @doc automate storage configuration and versioning.
%% @end
%%%-------------------------------------------------------------------

-module(automate_storage_configuration).

-exports([ get_versioning/0
         ]).

-include("./databases.hrl").
-include("./versioning.hrl").

-spec get_versioning() -> #database_version_progression{}.
get_versioning() ->
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

                  %% Running program threads table
                , #database_version_data{ database_name=?RUNNING_THREADS_TABLE
                                        , records=[ thread_id
                                                  , runner_pid
                                                  , parent_program_id
                                                  , instructions
                                                  , instruction_memory
                                                  , position
                                                  , stats
                                                  ]
                                        , record_name=running_program_thread_entry
                                        }

                  %% User programs table
                , #database_version_data{ database_name=?RUNNING_THREADS_TABLE
                                        , records=[ id
                                                  , user_id
                                                  , program_name
                                                  , program_type
                                                  , program_parsed
                                                  , program_orig
                                                  ]
                                        , record_name=user_program_entry
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

    { base=Version_1
    , updates=[]
    }.
