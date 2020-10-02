%%% @doc
%%% Automate bot thread linking.
%%% @end

-module(automate_bot_engine_link_threads_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

-define(APPLICATION, automate_bot_engine).
-define(TEST_NODES, [node()]).
-define(TEST_SERVICE, automate_service_registry_test_service:get_uuid()).
-define(TEST_SERVICE_ACTION, test_action).
-define(UTILS, automate_bot_engine_test_utils).

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
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    [ { "[Bot engine][Link threads] Link UTC-hour operation", fun thread_link_utc_hour/0 }
    , { "[Bot engine][Link threads] Link UTC-minute operation", fun thread_link_utc_minute/0 }
    , { "[Bot engine][Link threads] Link UTC-seconds operation", fun thread_link_utc_seconds/0 }
    ].


%%%% Bot runner
thread_link_utc_hour() ->
    TimeUuid = automate_services_time:get_uuid(),
    thread_link(<<"time_get_utc_hour">>, get_utc_hour, TimeUuid).

thread_link_utc_minute() ->
    TimeUuid = automate_services_time:get_uuid(),
    thread_link(<<"time_get_utc_minute">>, get_utc_minute, TimeUuid).

thread_link_utc_seconds() ->
    TimeUuid = automate_services_time:get_uuid(),
    thread_link(<<"time_get_utc_seconds">>, get_utc_seconds, TimeUuid).

thread_link(OrigCall, ResultAction, ServiceId) ->
    %% Program creation
    {ok, ChannelId} = automate_channel_engine:create_channel(),
    {Username, ProgramName, ProgramId} = create_anonymous_program(),

    %% Launch program
    Blocks = [[ ?UTILS:monitor_program_trigger(ChannelId)
              | wait_and_print(OrigCall)]],

    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type= <<"scratch_program">>
                                          , parsed=#{ <<"blocks">> => Blocks
                                                    , <<"variables">> => []
                                                    }
                                          , orig= <<"">>
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, wait_for_program_alive(ProgramId, 10, 100)),

    {ok, ProgramPid} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid)),

    %% Trigger sent, thread is spawned
    ProgramPid ! {channel_engine, ChannelId, #{ ?CHANNEL_MESSAGE_CONTENT => start }},
    ok = wait_for_check_ok(
           fun() ->
                   case automate_storage:get_threads_from_program(ProgramId) of
                       {ok, [ThreadId]} ->
                           case automate_storage:get_thread_from_id(ThreadId) of
                               {ok, #running_program_thread_entry{runner_pid=undefined}} ->
                                   false;
                               {ok, _} -> true
                           end;
                       _ ->
                           false
                   end
           end, 10, 100),

    %% Check that thread is alive
    {ok, [ThreadId]} = automate_storage:get_threads_from_program(ProgramId),
    {ok, #running_program_thread_entry{instructions=Program,
                                       runner_pid=ThreadRunnerId}} = automate_storage:get_thread_from_id(ThreadId),
    ?assert(is_process_alive(ThreadRunnerId)),

    %% Get second block, first argument, <<"type">>
    ?assertMatch(#{ ?ARGUMENTS :=
                       [ #{ ?TYPE := ?VARIABLE_BLOCK
                          , ?VALUE := [ #{ ?TYPE := ?COMMAND_CALL_SERVICE
                                         , ?ARGUMENTS :=
                                               #{ ?SERVICE_ACTION := ResultAction
                                                , ?SERVICE_ID := ServiceId
                                                , ?SERVICE_CALL_VALUES :=
                                                      #{ ?TYPE := OrigCall
                                                       }
                                                }
                                         }
                                      ]
                          }]},
                 lists:nth(2, Program)).


%%====================================================================
%% Util functions
%%====================================================================
create_anonymous_program() ->

    {Username, _UserId} = create_random_user(),

    ProgramName = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    {ok, ProgramId} = automate_storage:create_program(Username, ProgramName),
    {Username, ProgramName, ProgramId}.


create_random_user() ->
    Username = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Password = undefined,
    Email = binary:list_to_bin(uuid:to_string(uuid:uuid4())),

    {ok, UserId} = automate_storage:create_user(Username, Password, Email, ready),
    {Username, UserId}.

wait_for_program_alive(Pid, 0, SleepTime) ->
    {error, timeout};

wait_for_program_alive(ProgramId, TestTimes, SleepTime) ->
    case automate_storage:get_program_pid(ProgramId) of
        {ok, _} ->
            ok;
        {error, not_running} ->
            timer:sleep(SleepTime),
            wait_for_program_alive(ProgramId, TestTimes - 1, SleepTime)
    end.


wait_for_check_ok(Check, 0, SleepTime) ->
    {error, timeout};
wait_for_check_ok(Check, TestTimes, SleepTime) ->
    case Check() of
        true -> ok;
        false ->
            timer:sleep(SleepTime),
            wait_for_check_ok(Check, TestTimes - 1, SleepTime)
    end.

wait_and_print(X) -> [#{<<"args">> => [#{<<"type">> => <<"constant">>,
                                         <<"value">> => <<"10">>}]
                       , <<"contents">> => []
                       , <<"type">> => <<"control_wait">>
                       },
                      #{<<"args">> => [#{ ?TYPE => ?VARIABLE_BLOCK
                                        , ?VALUE => [ #{ <<"type">> => X
                                                       }
                                                    ]
                                        }
                                      ]
                       , <<"contents">> => []
                       , <<"type">> => ?COMMAND_LOG_VALUE
                       }].
