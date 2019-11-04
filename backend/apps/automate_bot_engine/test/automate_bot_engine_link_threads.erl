%%% @doc
%%% Automate bot engine tests.
%%% @end

-module(automate_bot_engine_link_threads).
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
-define(TEST_SERVICE, automate_service_registry_test_service:get_uuid()).
-define(TEST_SERVICE_ACTION, test_action).

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
    [ { "[Bot engine][Link threads] Link UTC-hour operation", fun thread_link_utc_hour/0 }
    ].


%%%% Bot runner
thread_link_utc_hour() ->
    %% Program creation
    {Username, ProgramName, ProgramId} = create_anonymous_program(),

    %% Launch program
    ?assertMatch({ok, ProgramId},
                 automate_storage:update_program(
                   Username, ProgramName,
                   #stored_program_content{ type=?JUST_WAIT_PROGRAM_TYPE
                                          , parsed=#{ <<"blocks">> => [[ ?JUST_WAIT_PROGRAM_TRIGGER
                                                                         | wait_and_print(<<"time_get_utc_hour">>)]]
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
    ProgramPid ! {channel_engine, ?JUST_WAIT_MONITOR_ID, #{ ?CHANNEL_MESSAGE_CONTENT => start }},
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

    %% Get thread content
    ?debugFmt("Program: \033[7m~p\033[0m~n", [Program]),
    %% Get second block, first argument, <<"type">>
    TimeUuid = automate_services_time:get_uuid(),
    Call = <<"services.", TimeUuid/binary, ".get_utc_hour">>,
    ?assertMatch(#{<<"args">> :=
                       [ #{ <<"type">> := Call
                          }]},
                 lists:nth(2, Program)).


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

    {ok, UserId} = automate_storage:create_user(Username, Password, Email),
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
                      #{<<"args">> => [#{<<"type">> => X
                                        , <<"args">> => []
                                        , <<"contents">> => []
                                        }]
                       , <<"contents">> => []
                       , <<"type">> => <<"control_print">>
                       }].
