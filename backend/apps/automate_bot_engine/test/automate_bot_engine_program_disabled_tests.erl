%%% @doc
%%% Automate bot engine tests.
%%% @end

-module(automate_bot_engine_program_disabled_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("just_wait_program.hrl").

-define(APPLICATION, automate_bot_engine).
-define(TEST_NODES, [node()]).
-define(TEST_MONITOR, <<"__test_monitor__">>).
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
    [ { "[Bot runner - Disable program] Start a program, launch a thread, disable program, the program must continue running"
      , fun start_program_launch_thread_and_disable_program_it_continues/0 }
    , { "[Bot runner - Disable program2] Create a program, disable it and try to launch a command"
      , fun start_program_and_disable_it_no_commands/0 }
    , { "[Bot runner - Disable program3] Create a program, disable it, enable it and launch a command"
      , fun start_program_disable_enable_and_launch_command/0 }
    ].


%%%% Bot runner
start_program_launch_thread_and_disable_program_it_continues() ->
    %% Sequence
    %%
    %% Test (this)  *---+--------------+.............+-----+---------+...........+---------+----------------+
    %%                  ↓              ↓             ↑     ↓         ↓           ↑         ↓                ↓
    %%                 Creates    Sends trigger  Confirms  ↓      Stop threads  Confirms  Check alive     Check alive
    %%                  ↓              ↓             ↑     ↓         ↓           ↑         ↓                ↓
    %% Program          *..............+-+-----------+....( )........+-+---------+.........YES.............( )...
    %%                                   ↓                 ↓           ↓                                    ↓
    %%                                Creates        Check alive     Stop                                 Check alive
    %%                                   ↓                 ↓           ↓                                    ↓
    %% Thread                            *-- wait ........YES..........X                                    NO

    %% Program creation
    {Username, ProgramName, ProgramId} = create_anonymous_program(),
    {ok, ChannelId} = automate_channel_engine:create_channel(),

    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=?JUST_WAIT_PROGRAM_TYPE
                                          , parsed=#{ <<"blocks">> => [[ ?UTILS:monitor_program_trigger(ChannelId)
                                                                       | ?JUST_WAIT_PROGRAM_INSTRUCTIONS ]]
                                                    , <<"variables">> => ?JUST_WAIT_PROGRAM_VARIABLES
                                                    }
                                          , orig=?JUST_WAIT_PROGRAM_ORIG
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, wait_for_program_alive(ProgramId, 10, 100)),

    {ok, ProgramPid} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid)),

    %% Trigger sent, thread is spawned
    ProgramPid ! {channel_engine, ChannelId, #{ ?CHANNEL_MESSAGE_CONTENT => start }},
    ok = wait_for_check_ok(fun() ->
                                   case automate_storage:get_threads_from_program(ProgramId) of
                                       {ok, [ThreadId]} ->
                                           case automate_storage:get_thread_from_id(ThreadId) of
                                               {ok, #running_program_thread_entry{runner_pid=undefined}} ->
                                                   io:fwrite("UNDEF~n"),
                                                   false;
                                               {ok, _} -> true
                                           end;
                                       _ ->
                                           false
                                   end
                           end, 10, 100),

    %% Check that thread is alive
    {ok, [ThreadId]} = automate_storage:get_threads_from_program(ProgramId),
    {ok, #running_program_thread_entry{runner_pid=ThreadRunnerId}} = automate_storage:get_thread_from_id(ThreadId),
    ?assert(is_process_alive(ThreadRunnerId)),

    %% Disable program
    ok = automate_bot_engine:change_program_status(ProgramId, false),

    %% Check that program is alive
    {ok, ProgramPid2} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid2)),

    %% Check that thread is dead
    wait_for_check_ok(fun() ->
                              case automate_storage:get_threads_from_program(ProgramId) of
                                  {ok, []} -> true;
                                  _ -> false
                              end
                      end, 10, 100),
    {ok, FinishedTreads} = automate_storage:get_threads_from_program(ProgramId),
    ?assert(length(FinishedTreads) == 1),

    ok.

start_program_and_disable_it_no_commands() ->
    %% Sequence
    %%
    %% Test (this)  *---+-----------+...........+---------+--→ OK
    %%                  ↓           ↓           ↑         ↓
    %%                 Creates   Stop threads  Confirms  Check alive
    %%                  ↓           ↓           ↑         ↓
    %% Program          *...........+-----------+.........YES

    {Username, ProgramName, ProgramId} = create_anonymous_program(),
    {ok, ChannelId} = automate_channel_engine:create_channel(),

    %% Program creation
    TriggerMonitorSignal = { ?TRIGGERED_BY_MONITOR
                           , { ChannelId, #{ ?CHANNEL_MESSAGE_CONTENT => start }}},

    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=?JUST_WAIT_PROGRAM_TYPE
                                          , parsed=#{ <<"blocks">> => [[ ?UTILS:monitor_program_trigger(ChannelId)
                                                                       | ?JUST_WAIT_PROGRAM_INSTRUCTIONS ]]
                                                    , <<"variables">> => ?JUST_WAIT_PROGRAM_VARIABLES
                                                    }
                                          , orig=?JUST_WAIT_PROGRAM_ORIG
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, wait_for_program_alive(ProgramId, 10, 100)),

    {ok, ProgramPid} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid)),

    %% Disable program
    ok = automate_bot_engine:change_program_status(ProgramId, false),

    %% Check that program is alive
    {ok, ProgramPid2} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid2)),

    %% Trigger sent, thread is spawned
    ProgramPid ! {channel_engine, ChannelId, #{ ?CHANNEL_MESSAGE_CONTENT => start }},
    timer:sleep(1000),

    {ok, Threads} = automate_storage:get_threads_from_program(ProgramId),
    ?assert(length(Threads) == 0),

    ok.

start_program_disable_enable_and_launch_command()->
    %% Program creation
    {Username, ProgramName, ProgramId} = create_anonymous_program(),
    {ok, ChannelId} = automate_channel_engine:create_channel(),

    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=?JUST_WAIT_PROGRAM_TYPE
                                          , parsed=#{ <<"blocks">> => [[ ?UTILS:monitor_program_trigger(ChannelId)
                                                                       | ?JUST_WAIT_PROGRAM_INSTRUCTIONS ]]
                                                    , <<"variables">> => ?JUST_WAIT_PROGRAM_VARIABLES
                                                    }
                                          , orig=?JUST_WAIT_PROGRAM_ORIG
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, wait_for_program_alive(ProgramId, 10, 100)),

    {ok, ProgramPid} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid)),

    %% Disable program
    ok = automate_bot_engine:change_program_status(ProgramId, false),

    %% Check that program is alive
    {ok, ProgramPid2} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid2)),

    ok = automate_bot_engine:change_program_status(ProgramId, true),

    %% Trigger sent, thread is spawned
    ProgramPid ! {channel_engine, ChannelId, #{ ?CHANNEL_MESSAGE_CONTENT => start }},
    ok = wait_for_check_ok(fun() ->
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
    {ok, #running_program_thread_entry{runner_pid=ThreadRunnerId}} = automate_storage:get_thread_from_id(ThreadId),
    ?assert(is_process_alive(ThreadRunnerId)),

    {ok, FinishedTreads} = automate_storage:get_threads_from_program(ProgramId),
    ?assert(length(FinishedTreads) == 1),

    ok.




%%====================================================================
%% Util functions
%%====================================================================
create_anonymous_program() ->

    {Username, UserId} = create_random_user(),

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
