%%% @doc
%%% Automate bot engine tests.
%%% @end

-module(automate_bot_engine_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").

%% Test data
-include("single_line_program.hrl").

-define(TEST_NODES, [node()]).
-define(TEST_MONITOR, <<"__test_monitor__">>).

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
    %% NodeName = node(),

    %% %% Use a custom node name to avoid overwriting the actual databases
    %% net_kernel:start([?MODULE, shortnames]),
    %% hivemind_app:install(),
    %% {ok, Pid} = hivemind_app:start(),

    %% {NodeName, Pid}.
    ok.

%% @doc App infrastructure teardown.
%% @end
%% stop({NodeName, Pid}) ->
stop(ok) ->
    %% hivemind_app:stop(Pid),
    %% ok = mnesia:delete_schema(?TEST_NODES),

    %% %% Restore the original node name
    %% net_kernel:start([NodeName, shortnames]).
    ok.

tests(_SetupResult) ->
    [ {"[Bot runner][Initialization] Single line program initialization", fun single_line_program_initialization/0}
    , {"[Bot runner][Signals] Wait for channel signal", fun wait_for_channel_signal/0}
    , {"[Bot runner][Resolution] Constant argument resolution", fun constant_argument_resolution/0}
    , {"[Bot runner][Triggers] Trigger thread with channel signal", fun trigger_thread_with_channel_signal/0}
    , {"[Bot runner][Threads] Run a thread a single tick", fun run_thread_single_tick/0}
    ].


%%%% Bot runner
%% Initialization
single_line_program_initialization() ->
    Program  = ?SINGLE_LINE_PROGRAM,
    Expected = ?SINGLE_LINE_PROGRAM_INITIALIZATION,

    ?assertMatch({ok, Expected},
                 automate_bot_engine_program_decoder:initialize_program(?SINGLE_LINE_PROGRAM_ID, Program)).

%% Signals
wait_for_channel_signal() ->
    Program = #program_state{ triggers=[#program_trigger{ condition=#{ ?TYPE => ?WAIT_FOR_MONITOR
                                                                     , ?ARGUMENTS =>
                                                                           #{ ?MONITOR_ID => ?TEST_MONITOR
                                                                            , ?MONITOR_EXPECTED_VALUE => ?MONITOR_ANY_VALUE
                                                                            }
                                                                     }
                                                        }]},
    Expected = [?TRIGGERED_BY_MONITOR],

    ?assertMatch({ok, Expected},
                 automate_bot_engine_triggers:get_expected_signals(Program)).

%% Argument resolution
constant_argument_resolution() ->
    Value = example,
    ?assertMatch({ok, Value}, automate_bot_engine_variables:resolve_argument(#{ ?TYPE => ?VARIABLE_CONSTANT
                                                                              , ?VALUE => Value
                                                                              })).

%% Threads
trigger_thread_with_channel_signal() ->
    Program = #program_state{ triggers=[#program_trigger{
                                           condition=#{ ?TYPE => ?WAIT_FOR_MONITOR
                                                      , ?ARGUMENTS => #{ ?MONITOR_ID => ?TEST_MONITOR
                                                                       , ?MONITOR_EXPECTED_VALUE =>
                                                                             #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                              , ?VALUE => example
                                                                              }
                                                                       }
                                                      }
                                          , subprogram=[#{ ?TYPE => example }]
                                          }]},

    {ok, [Thread]} = automate_bot_engine_triggers:get_triggered_threads(Program, { ?TRIGGERED_BY_MONITOR
                                                                                 , { ?TEST_MONITOR, example }}),
    ?assertMatch(#program_thread{ position=[1], program=[#{ ?TYPE := example }] }, Thread).

run_thread_single_tick() ->
    WaitForMonitorInstruction = #{ ?TYPE => ?WAIT_FOR_MONITOR
                                 , ?ARGUMENTS => #{ ?MONITOR_ID => ?TEST_MONITOR
                                                  , ?MONITOR_EXPECTED_VALUE => #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                , ?VALUE => example
                                                                                }
                                                  }
                                 },
    CallServiceInstruction = #{ ?TYPE => ?COMMAND_CALL_SERVICE
                              , ?ARGUMENTS => [#{ ?TYPE => ?VARIABLE_CONSTANT
                                                , ?VALUE => answer
                                                }
                                              ]
                              },
    TriggerMonitorSignal = { ?TRIGGERED_BY_MONITOR
                           , { ?TEST_MONITOR, example }},

    Program = #program_state{ triggers=[#program_trigger{ condition=WaitForMonitorInstruction
                                                        , subprogram=[CallServiceInstruction]
                                                        }]},

    {ok, [Thread]} = automate_bot_engine_triggers:get_triggered_threads(Program, TriggerMonitorSignal),

    %% Unexpected signal (for the thread already started), does not run
    #program_thread{ position=[1], program=[CallServiceInstruction] } = Thread,
    {ok, {Ran1, NotRun1}} = automate_bot_engine_operations:run_threads([Thread], Program#program_state{ threads=[Thread] },
                                                                       {TriggerMonitorSignal, none}),
    ?assertMatch([], Ran1),
    ?assertMatch([#program_thread{position=[1]}], NotRun1),

    %% Expected signal, does run
    {ok, {Ran2, NotRun2}} = automate_bot_engine_operations:run_threads([Thread], Program#program_state{ threads=[Thread] },
                                                                       {?SIGNAL_PROGRAM_TICK, none}),
    ?assertMatch([#program_thread{position=[]}], Ran2),
    ?assertMatch([], NotRun2).
