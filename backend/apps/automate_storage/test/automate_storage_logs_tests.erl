%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_storage_logs_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, automate_storage).
%% Data structures
-include("../../automate_storage/src/records.hrl").

-define(TEST_NODES, [node()]).

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

    {ok, Pid} = application:ensure_all_started(automate_storage),

    {NodeName, Pid}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName, _Pid}) ->
    application:stop(automate_storage),

    ok.

%%====================================================================
%% Tests
%%====================================================================
tests(_SetupResult) ->
    [ {"[Storage program logs] Check program error log watermarks", fun test_error_logs_watermarks/0}
    , {"[Storage program logs] Check user log watermarks", fun test_user_logs_watermarks/0}
    ].

test_error_logs_watermarks() ->
    {LowWatermark, HighWatermark} = automate_configuration:get_program_logs_watermarks(),
    ProgramId = <<"automate_storage_logs_tests/test_watermarks">>,

    %% Generate and insert first batch
    Entries = lists:map(fun(Idx) -> gen_log_with_id(Idx, ProgramId) end, lists:seq(1, HighWatermark)),
    ok = lists:foreach(fun automate_storage:log_program_error/1, Entries),

    %% Check that it reaches up until HighWatermark
    {ok, FilledLogs} = automate_storage:get_logs_from_program_id(ProgramId),
    ?assertMatch(HighWatermark, length(FilledLogs)),

    %% Add the one log that overflows the watermark
    automate_storage:log_program_error(gen_log_with_id(HighWatermark + 1, ProgramId)),

    %% Check that now the LowWatermark is in use
    {ok, EmptiedLogs} = automate_storage:get_logs_from_program_id(ProgramId),
    ?assertMatch(LowWatermark, length(EmptiedLogs)),

    %% All logs present are the more recent ones
    Deleted = HighWatermark - LowWatermark,
    Latest = HighWatermark + 1,
    lists:foreach(fun(#user_program_log_entry{ event_time=Idx }) ->
                          ?assert(Idx > (Latest - Deleted))
                  end, EmptiedLogs).

test_user_logs_watermarks() ->
    {LowWatermark, HighWatermark} = automate_configuration:get_program_logs_watermarks(),
    ProgramId = <<"automate_storage_logs_tests/test_watermarks">>,

    %% Generate and insert first batch
    Entries = lists:map(fun(Idx) -> gen_user_log_with_id(Idx, ProgramId) end, lists:seq(1, HighWatermark)),
    ok = lists:foreach(fun automate_storage:add_user_generated_log/1, Entries),

    %% Check that it reaches up until HighWatermark
    {ok, FilledLogs} = automate_storage:get_user_generated_logs(ProgramId),
    ?assertMatch(HighWatermark, length(FilledLogs)),

    %% Add the one log that overflows the watermark
    automate_storage:add_user_generated_log(gen_user_log_with_id(HighWatermark + 1, ProgramId)),

    %% Check that now the LowWatermark is in use
    {ok, EmptiedLogs} = automate_storage:get_user_generated_logs(ProgramId),
    ?assertMatch(LowWatermark, length(EmptiedLogs)),

    %% All logs present are the more recent ones
    Deleted = HighWatermark - LowWatermark,
    Latest = HighWatermark + 1,
    lists:foreach(fun(#user_generated_log_entry{ event_time=Idx }) ->
                          ?assert(Idx > (Latest - Deleted))
                  end, EmptiedLogs).

%%====================================================================
%% Internal
%%====================================================================
gen_log_with_id(Idx, ProgramId) ->
    #user_program_log_entry{ program_id=ProgramId
                           , thread_id=none
                           , owner=none
                           , block_id=undefined
                           , event_data=Idx
                           , event_message= <<"test">>
                           , event_time=Idx
                           , severity=error
                           , exception_data=none
                           }.

gen_user_log_with_id(Idx, ProgramId) ->
    #user_generated_log_entry{ program_id=ProgramId
                             , block_id=undefined
                             , severity=error
                             , event_message= <<"test">>
                             , event_time=Idx
                             }.
