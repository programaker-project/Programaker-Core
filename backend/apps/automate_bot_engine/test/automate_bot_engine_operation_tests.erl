%%% Automate bot engine getters tests.
%%% @end

-module(automate_bot_engine_operation_tests).
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

    %% %% Use a custom node name to avoid overwriting the actual databases
    %% net_kernel:start([testing, shortnames]),

    %% {ok, Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][Misc. Operations] Log value", fun test_log_value/0}
    ].

%%%% Operations
test_log_value() ->
    #program_thread{program_id=Pid}=Thread=empty_thread(),
    {ran_this_tick, _Thread2} = automate_bot_engine_operations:run_instruction(
                                  #{ ?TYPE => ?COMMAND_LOG_VALUE
                                   , ?ARGUMENTS => [ constant_val(<<"test line">>)
                                                   ]
                                   }, Thread, {?SIGNAL_PROGRAM_TICK, test}),
    Logs = automate_bot_engine:get_user_generated_logs(Pid),
    ?assertMatch({ok, [#user_generated_log_entry{event_message= <<"test line">>}]}, Logs).

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.

empty_thread() ->
    #program_thread{ position = [1]
                   , program=[undefined]
                   , global_memory=#{}
                   , instruction_memory=#{}
                   , program_id=binary:list_to_bin(uuid:to_string(uuid:uuid4()))
                   , thread_id=undefined
                   }.
