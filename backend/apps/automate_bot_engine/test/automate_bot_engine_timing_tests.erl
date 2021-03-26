%%% Automate bot engine getters tests.
%%% @end

-module(automate_bot_engine_timing_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").
-include("../../automate_services_time/src/definitions.hrl").

%% Test data
-include("single_line_program.hrl").

-define(APPLICATION, automate_bot_engine).
-define(WAIT_PER_INSTRUCTION, 100).  %% Milliseconds
%% Note, if waiting per instruction takes too much time consider adding a method
%% which checks periodically.
-define(UTILS, automate_bot_engine_test_utils).
-define(BRIDGE_UTILS, automate_service_port_engine_test_utils).

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

    {ok, _} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% ok = application:stop(automate_service_port_engine),
    %% ok = application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][Timing] Wait for time signal", fun wait_for_simple_time_signal/0}
    , {"[Bot engine][Timing] Wait for time signal without Timezone change", fun wait_for_time_signal_on_no_timezone_change/0}
    , {"[Bot engine][Timing] Wait for time signal on Timezone change", fun wait_for_time_signal_on_timezone_change/0}
    ].

%%%% Operations
wait_for_simple_time_signal() ->
    {_Username, _ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [constant_val(<<"before">>)] }
                                                       , { ?COMMAND_WAIT_FOR_NEXT_VALUE,
                                                           [ ?UTILS:block_val({ ?WAIT_FOR_MONITOR
                                                                              , #{ ?FROM_SERVICE => automate_services_time:get_uuid()
                                                                                 , <<"key">> => <<"utc_time">>
                                                                                 }
                                                                              })
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    %% Launch
    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),

    %% Wait ~2 seconds, should be enough for the time signal to arrive
    timer:sleep(3000),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"before">>, <<"after">>], MsgsAfter),
    ok.

wait_for_time_signal_on_no_timezone_change() ->
    TestTimezone = "Europe/Madrid",

    %% This time (UTC) corresponds to 8:59:58 next day on the timezone.
    %% Two seconds later, the hour will be the tested 09:00:00 .
    TestTime = {{2021, 03, 26}, {7, 59, 58}},

    ok = automate_testing:set_corrected_time(TestTime),

    ProgramData = [ { ?WAIT_FOR_MONITOR_COMMAND,
                      #{ ?MONITOR_ID => #{ ?FROM_SERVICE => ?TIME_SERVICE_UUID  }
                       , ?MONITOR_EXPECTED_VALUE => #{ <<"type">> => <<"constant">>
                                                     , <<"value">> => <<"09:00:00">>
                                                     }
                       , <<"timezone">> => TestTimezone
                       }
                    }
                  , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                  ],

    {Username, ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),
    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=scratch_program
                                          , parsed=#{ <<"blocks">> => [ ?UTILS:build_ast(ProgramData) ]
                                                    , <<"variables">> => []
                                                    }
                                          , orig= <<"*test*">>
                                          , pages=#{}
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Wait ~3 seconds, should be enough for the time signal to arrive
    timer:sleep(3000),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    automate_testing:unset_corrected_time(),

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"after">> ], MsgsAfter),
    ok.

wait_for_time_signal_on_timezone_change() ->
    TestTimezone = "Europe/Madrid",

    %% This time (UTC) corresponds to 01:59:58 next day on the timezone.
    %% Two seconds later, the hour will jump to 03:00:00 .
    TestTime = {{2021, 03, 28}, {00, 59, 59}},

    ok = automate_testing:set_corrected_time(TestTime),

    ProgramData = [ { ?WAIT_FOR_MONITOR_COMMAND,
                      #{ ?MONITOR_ID => #{ ?FROM_SERVICE => ?TIME_SERVICE_UUID  }
                       , ?MONITOR_EXPECTED_VALUE => #{ <<"type">> => <<"constant">>
                                                     , <<"value">> => <<"03:00:01">>
                                                     }
                       , <<"timezone">> => TestTimezone
                       }
                    }
                  , { ?COMMAND_LOG_VALUE, [constant_val(<<"after">>)] }
                  ],

    {Username, ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),
    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=scratch_program
                                          , parsed=#{ <<"blocks">> => [ ?UTILS:build_ast(ProgramData) ]
                                                    , <<"variables">> => []
                                                    }
                                          , orig= <<"*test*">>
                                          , pages=#{}
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    {ok, LogsBefore} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsBefore = [ M || #user_generated_log_entry{event_message=M} <- LogsBefore ],

    io:fwrite("Immediate logs: ~p~n", [MsgsBefore]),
    ?assertMatch([ ], MsgsBefore),

    %% Wait ~4 seconds, should be enough for the time signal to arrive
    timer:sleep(4000),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    automate_testing:unset_corrected_time(),

    io:fwrite("Logs after signal: ~p~n", [MsgsAfter]),
    ?assertMatch([ <<"after">> ], MsgsAfter),

    %% ?assertEqual(this_should_not_work_yet, false),
    ok.

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.
