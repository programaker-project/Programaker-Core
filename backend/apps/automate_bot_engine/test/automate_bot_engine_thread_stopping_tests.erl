%%% @doc
%%% Automate bot engine tests.
%%% @end

-module(automate_bot_engine_thread_stopping_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("just_wait_program.hrl").

-define(APPLICATION, automate_bot_engine).
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
    application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    [ { "[Bot runner - Stop threads] Start a thread and stop it, the program must continue running"
      , fun start_thread_and_stop_threads_continues/0 }
    , { "[Bot runner - Stop threads] Create a program and stop it's threads (none). Nothing happens"
      , fun start_program_and_stop_threads_nothing/0 }
    ].


%%%% Bot runner
start_thread_and_stop_threads_continues() ->
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
    {Username, ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),

    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=?JUST_WAIT_PROGRAM_TYPE
                                          , parsed=#{ <<"blocks">> => [[ ?JUST_WAIT_PROGRAM_TRIGGER
                                                                         | ?JUST_WAIT_PROGRAM_INSTRUCTIONS ]]
                                                    , <<"variables">> => ?JUST_WAIT_PROGRAM_VARIABLES
                                                    }
                                          , orig=?JUST_WAIT_PROGRAM_ORIG
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, ?UTILS:wait_for_program_alive(ProgramId, 10, 100)),

    {ok, ProgramPid} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid)),

    %% Trigger sent, thread is spawned
    ProgramPid ! {channel_engine, ?JUST_WAIT_MONITOR_ID, #{ ?CHANNEL_MESSAGE_CONTENT => start }},
    ok = ?UTILS:wait_for_check_ok(
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
    {ok, #running_program_thread_entry{runner_pid=ThreadRunnerId}} = automate_storage:get_thread_from_id(ThreadId),
    ?assert(is_process_alive(ThreadRunnerId)),

    %% Stop threads
    ok = automate_rest_api_backend:stop_program_threads(undefined, ProgramId),

    %% Check that program is alive
    {ok, ProgramPid2} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid2)),

    %% Check that thread is dead
    ?UTILS:wait_for_check_ok(
      fun() ->
              case automate_storage:get_threads_from_program(ProgramId) of
                  {ok, []} -> true;
                  _ -> false
              end
      end, 10, 100),
    {ok, FinishedTreads} = automate_storage:get_threads_from_program(ProgramId),
    ?assert(length(FinishedTreads) == 0),
    ok.

start_program_and_stop_threads_nothing() ->
    %% Sequence
    %%
    %% Test (this)  *---+-----------+...........+---------+--→ OK
    %%                  ↓           ↓           ↑         ↓
    %%                 Creates   Stop threads  Confirms  Check alive
    %%                  ↓           ↓           ↑         ↓
    %% Program          *...........+-----------+.........YES

    %% Program creation
    TriggerMonitorSignal = { ?TRIGGERED_BY_MONITOR
                           , { ?JUST_WAIT_MONITOR_ID, #{ ?CHANNEL_MESSAGE_CONTENT => start }}},

    {Username, ProgramName, ProgramId} = ?UTILS:create_anonymous_program(),

    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=?JUST_WAIT_PROGRAM_TYPE
                                          , parsed=#{ <<"blocks">> => [[ ?JUST_WAIT_PROGRAM_TRIGGER
                                                                         | ?JUST_WAIT_PROGRAM_INSTRUCTIONS ]]
                                                    , <<"variables">> => ?JUST_WAIT_PROGRAM_VARIABLES
                                                    }
                                          , orig=?JUST_WAIT_PROGRAM_ORIG
                                          })),

    ?assertMatch(ok, automate_bot_engine_launcher:update_program(ProgramId)),

    %% Check that program id alive
    ?assertMatch(ok, ?UTILS:wait_for_program_alive(ProgramId, 10, 100)),

    {ok, ProgramPid} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid)),

    %% Stop threads
    ok = automate_rest_api_backend:stop_program_threads(undefined, ProgramId),

    %% Check that program is alive
    {ok, ProgramPid2} = automate_storage:get_program_pid(ProgramId),
    ?assert(is_process_alive(ProgramPid2)),

    ok.
