%%% @doc
%%% Automate bot engine operation tests.
%%% @end

-module(automate_bot_engine_operations_tests).
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
-define(EMPTY_THREAD, #program_thread{ position = [1]
                                     , program=[undefined]
                                     , global_memory=#{}
                                     , instruction_memory=#{}
                                     , program_id=undefined
                                     }).

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
    [ {"[Bot engine][Get-result] Compare equal integer strings as int", fun compare_bin_int_bin_int_eq_true/0}
    , {"[Bot engine][Get-result] Compare different integer strings as int", fun compare_bin_int_bin_int_eq_false/0}
    , {"[Bot engine][Get-result] Compare equal string to int", fun compare_bin_in_raw_int_eq_true/0}
    ].


compare_bin_int_bin_int_eq_true() ->
    R = automate_bot_engine_operations:get_result(#{ ?TYPE => ?COMMAND_EQUALS
                                                   , ?ARGUMENTS => [ constant_val(<<"1">>)
                                                                   , constant_val(<<"1">>)
                                                                   ]
                                                   }, ?EMPTY_THREAD),
    ?assertMatch({ok, true}, R).

compare_bin_int_bin_int_eq_false() ->
    R = automate_bot_engine_operations:get_result(#{ ?TYPE => ?COMMAND_EQUALS
                                                   , ?ARGUMENTS => [ constant_val(<<"2">>)
                                                                   , constant_val(<<"3">>)
                                                                   ]
                                                   }, ?EMPTY_THREAD),
    ?assertMatch({ok, false}, R).

compare_bin_in_raw_int_eq_true() ->
    R = automate_bot_engine_operations:get_result(#{ ?TYPE => ?COMMAND_EQUALS
                                                   , ?ARGUMENTS => [ constant_val(<<"1">>)
                                                                   , constant_val(1)
                                                                   ]
                                                   }, ?EMPTY_THREAD),
    ?assertMatch({ok, true}, R).

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.
