%%% @doc
%%% Automate bot thread linking.
%%% @end

-module(automate_bot_engine_trigger_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data

-define(APPLICATION, automate_bot_engine).
-define(TEST_NODES, [node()]).
-define(TEST_SERVICE, automate_service_registry_test_service:get_uuid()).
-define(TEST_SERVICE_ACTION, test_action).

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
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    [ { "[Bot engine][Trigger tests] Test save-to", fun save_to_test/0 }
    , { "[Bot engine][Trigger tests] Trigger with time", fun trigger_with_time/0 }
    , { "[Bot engine][Trigger tests] Trigger with Tz time", fun trigger_with_tz_time/0 }
    ].



save_to_test() ->
    %% Create service port
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-1-owner"])},
    ServicePortName = iolist_to_binary([Prefix, "-test-1-service-port"]),
    {ok, ServicePortId} = automate_service_port_engine:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        #{ <<"type">> => <<"CONFIGURATION">>
                                                         , <<"value">> => Configuration
                                                         }),
    {ok, _} = ?BRIDGE_UTILS:establish_connection(ServicePortId, OwnerUserId),

    %% Program creation
    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),

    %% Launch program
    Blocks = [ #{ <<"type">> => iolist_to_binary([ "services."
                                                 , ServicePortId
                                                 , ".on_new_message"
                                                 ])

                , ?ARGUMENTS => #{ <<"key">> => <<"on_new_message">>
                                 , <<"subkey">> => #{ ?TYPE => ?VARIABLE_CONSTANT
                                                    , ?VALUE => <<"correct">>
                                                    }
                                 , <<"monitor_save_value_to">> =>
                                       #{ <<"type">> => <<"variable">>
                                        , <<"value">> => <<"var">>
                                        }
                                 }
                , <<"save_to">> => %% This is a possible error we have to handle
                      #{ <<"index">> => 1
                       , <<"type">> => <<"argument">>
                       }
                }
             , #{ <<"type">>  => ?COMMAND_LOG_VALUE
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_VARIABLE
                                   , ?VALUE => <<"var">>
                                   }
                                ]
                }
             ],

    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program_by_id(
                   ProgramId, #stored_program_content{ type= <<"scratch_program">>
                                                     , parsed=#{ <<"blocks">> => [ Blocks ]
                                                               , <<"variables">> => []
                                                               }
                                                     , orig=undefined
                                                     , pages=#{}
                                                     })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, ?UTILS:wait_for_program_alive(ProgramId, 10, 100)),

    %% Send signal
    ok = automate_service_port_engine:from_service_port(ServicePortId, OwnerUserId,
                                                        #{ <<"type">> => <<"NOTIFICATION">>
                                                         , <<"key">> => <<"on_new_message">>
                                                         , <<"subkey">> => <<"correct">>
                                                         , <<"to_user">> => null
                                                         , <<"value">> => <<"sample value">>
                                                         , <<"content">> => <<"sample content">>
                                                         }),

    timer:sleep(?WAIT_PER_INSTRUCTION * 3),

    {ok, LogsWaited} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsWaited = [ M || #user_generated_log_entry{event_message=M} <- LogsWaited ],
    io:fwrite("Logs after signal: ~p~n", [MsgsWaited]),
    ?assertMatch([ <<"sample content">> ], MsgsWaited),
    ok.

trigger_with_time() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-2-owner"])},
    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),

    {_, { StartHour, StartMin, StartSec }} = calendar:now_to_datetime(erlang:timestamp()),

    {MegaSeconds, Seconds, MicroSeconds} = erlang:timestamp(),

    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:now_to_datetime({MegaSeconds, Seconds + 2, MicroSeconds}),
    %% Wait for the next second
    Value = binary:list_to_bin(lists:flatten(io_lib:format("~p:~p:~p", [Hour, Min, Sec]))),
    io:fwrite("Waiting for: ~p (~s)~n", [Value, Value]),
    Blocks = [ #{ <<"type">> => <<"wait_for_monitor">>
                , ?ARGUMENTS => #{ ?MONITOR_EXPECTED_VALUE => #{ ?TYPE => ?VARIABLE_CONSTANT
                                                               , ?VALUE => Value
                                                               }
                                 , ?MONITOR_ID => #{ ?FROM_SERVICE => automate_services_time:get_uuid() }
                                 }
                }
             , #{ <<"type">>  => ?COMMAND_LOG_VALUE
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_CONSTANT
                                   , ?VALUE => <<"after">>
                                   }
                                ]
                }
             ],

    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program_by_id(
                   ProgramId, #stored_program_content{ type= <<"scratch_program">>
                                                     , parsed=#{ <<"blocks">> => [ Blocks ]
                                                               , <<"variables">> => []
                                                               }
                                                     , orig=undefined
                                                     , pages=#{}
                                                     })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    io:fwrite("PID: ~p~n", [ProgramId]),

    %% Check that program is alive
    ?assertMatch(ok, ?UTILS:wait_for_program_alive(ProgramId, 10, 100)),

    %% Wait >2 seconds, should be enough for the time signal to arrive
    timer:sleep(4000),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    {_, {AfterHour, AfterMin, AfterSec}} = calendar:now_to_datetime(erlang:timestamp()),

    io:fwrite("Logs after ~p→~p: ~p~n", [{StartHour, StartMin, StartSec}, {AfterHour, AfterMin, AfterSec}, MsgsAfter]),
    ?assertMatch([ <<"after">>], MsgsAfter),
    ok.

trigger_with_tz_time() ->
    Prefix = erlang:atom_to_list(?MODULE),
    OwnerUserId = {user, iolist_to_binary([Prefix,"-test-3-owner"])},
    {ok, ProgramId} = ?UTILS:create_user_program(OwnerUserId),

    TestTimezone = <<"Etc/GMT+1">>,

    {_, { StartHour, StartMin, StartSec }} = calendar:now_to_datetime(erlang:timestamp()),

    {MegaSeconds, Seconds, MicroSeconds} = erlang:timestamp(),
    {_, { Hour, Min, Sec }} = qdate:to_date(TestTimezone, calendar:now_to_datetime({MegaSeconds, Seconds + 2, MicroSeconds})),

    TestTime = binary:list_to_bin(lists:flatten(io_lib:format("~p:~p:~p", [Hour, Min, Sec]))),


    io:fwrite("Waiting for: ~p (~s ~s)~n", [TestTime, TestTime, TestTimezone]),
    Blocks = [ #{ <<"type">> => <<"wait_for_monitor">>
                , ?ARGUMENTS => #{ ?MONITOR_EXPECTED_VALUE => #{ ?TYPE => ?VARIABLE_CONSTANT
                                                               , ?VALUE => TestTime
                                                               }
                                 , ?MONITOR_ID => #{ ?FROM_SERVICE => automate_services_time:get_uuid() }
                                 , <<"timezone">> => TestTimezone
                                 }
                }
             , #{ <<"type">>  => ?COMMAND_LOG_VALUE
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_CONSTANT
                                   , ?VALUE => <<"after">>
                                   }
                                ]
                }
             ],

    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program_by_id(
                   ProgramId, #stored_program_content{ type= <<"scratch_program">>
                                                     , parsed=#{ <<"blocks">> => [ Blocks ]
                                                               , <<"variables">> => []
                                                               }
                                                     , orig=undefined
                                                     , pages=#{}
                                                     })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    io:fwrite("PID: ~p~n", [ProgramId]),

    %% Check that program is alive
    ?assertMatch(ok, ?UTILS:wait_for_program_alive(ProgramId, 10, 100)),

    %% Wait >2 seconds, should be enough for the time signal to arrive
    timer:sleep(4000),
    {ok, LogsAfter} = automate_bot_engine:get_user_generated_logs(ProgramId),
    MsgsAfter = [ M || #user_generated_log_entry{event_message=M} <- LogsAfter ],

    {_, {AfterHour, AfterMin, AfterSec}} = calendar:now_to_datetime(erlang:timestamp()),

    io:fwrite("Logs after ~p→~p: ~p~n", [{StartHour, StartMin, StartSec}, {AfterHour, AfterMin, AfterSec}, MsgsAfter]),
    ?assertMatch([ <<"after">>], MsgsAfter),
    ok.
