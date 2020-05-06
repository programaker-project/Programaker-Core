%%% Automate bot engine getters tests.
%%% @end

-module(automate_bot_engine_forking_flows_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("single_line_program.hrl").

-define(APPLICATION, automate_bot_engine).
-define(TEST_NODES, [node()]).
-define(TEST_MONITOR, <<"__test_monitor__">>).
-define(TEST_SERVICE, automate_service_registry_test_service:get_uuid()).
-define(TEST_SERVICE_ACTION, test_action).
-define(WAIT_PER_INSTRUCTION, 100).  %% Milliseconds

%%====================================================================
%% Test API
%%====================================================================

session_manager_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

%% @doc App infrastructure setup.
%% @end
setup() ->
    NodeName = node(),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    {ok, _Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][Fork Operations] Simple fork (no join)", fun simple_fork_no_join/0}
    , {"[Bot engine][Fork Operations] Simple fork with join", fun simple_fork_with_join/0}
    ].

%%%% Operations
simple_fork_no_join() ->
    ExpectedLogs = [<<"first branch">>, <<"second branch">>], %% Note that they might be shuffled

    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_FORK_EXECUTION
                                         , ?CONTENTS => [ #{ ?CONTENTS => [ #{ ?TYPE => ?COMMAND_LOG_VALUE
                                                                             , ?ARGUMENTS => [ constant_val(<<"first branch">>)
                                                                                             ]
                                                                             }
                                                                          ]
                                                           }
                                                        , #{ ?CONTENTS => [ #{ ?TYPE => ?COMMAND_LOG_VALUE
                                                                             , ?ARGUMENTS => [ constant_val(<<"second branch">>)
                                                                                             ]
                                                                             }
                                                                          ]
                                                           }
                                                        ]
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    [ #user_generated_log_entry{event_message=First}
    , #user_generated_log_entry{event_message=Second}
    ] = Logs,

    io:fwrite("Logs: ~p~n", [[First, Second]]),
    io:fwrite("Expected: ~p~n", [ExpectedLogs]),
    ?assert(([First, Second] =:= ExpectedLogs)
            or ([Second, First] =:= ExpectedLogs)).

simple_fork_with_join() ->
    ExpectedLogs = [<<"first branch">>, <<"second branch">>, <<"joined">>], %% Note that the first two might be shuffled

    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_FORK_EXECUTION
                                         , ?CONTENTS => [ #{ ?CONTENTS => [ #{ ?TYPE => ?COMMAND_LOG_VALUE
                                                                             , ?ARGUMENTS => [ constant_val(<<"first branch">>)
                                                                                             ]
                                                                             }
                                                                          ]
                                                           }
                                                        , #{ ?CONTENTS => [ #{ ?TYPE => ?COMMAND_LOG_VALUE
                                                                             , ?ARGUMENTS => [ constant_val(<<"second branch">>)
                                                                                             ]
                                                                             }
                                                                          ]
                                                           }
                                                        ]
                                         }
                                      , #{ ?TYPE => ?COMMAND_LOG_VALUE
                                         , ?ARGUMENTS => [ constant_val(<<"joined">>)
                                                         ]
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 4),
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    [ #user_generated_log_entry{event_message=First}
    , #user_generated_log_entry{event_message=Second}
    , #user_generated_log_entry{event_message=Joined}
    ] = Logs,

    io:fwrite("Logs: ~p~n", [[First, Second]]),
    io:fwrite("Expected: ~p~n", [ExpectedLogs]),
    ?assert(([First, Second, Joined] =:= ExpectedLogs)
            or ([Second, First, Joined] =:= ExpectedLogs)).


%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.
