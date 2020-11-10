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
    [ {"[Bot engine][Misc. Operations] Log value", fun test_log_value/0}
    , {"[Bot engine][String operations] Text contains - Simple - True", fun text_contains_simple_true/0}
    , {"[Bot engine][String operations] Text contains - Simple - False", fun text_contains_simple_false/0}
    , {"[Bot engine][String operations] Text contains - Case insensitive  - True", fun text_contains_case_insensitive/0}
    , {"[Bot engine][String operations] Text contains - Accent insensitive  - True", fun text_contains_accent_insensitive/0}
    , {"[Bot engine][String operations] Text contains - Several Characters Map to same", fun text_contains_several_map_to_same/0}
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

%%%% Text contains operations
text_contains_simple_true() ->
    Thread = empty_thread(),
    {ok, Value, _} = automate_bot_engine_operations:get_result(
                                  #{ ?TYPE => ?COMMAND_STRING_CONTAINS
                                   , ?ARGUMENTS => [ constant_val(<<"this is a test string">>)
                                                   , constant_val(<<"test">>)
                                                   ]
                                   }, Thread),
    ?assertMatch(true, Value).

text_contains_simple_false() ->
    Thread = empty_thread(),
    {ok, Value, _} = automate_bot_engine_operations:get_result(
                    #{ ?TYPE => ?COMMAND_STRING_CONTAINS
                     , ?ARGUMENTS => [ constant_val(<<"this is a not a pass">>)
                                     , constant_val(<<"test">>)
                                     ]
                     }, Thread),
    ?assertMatch(false, Value).

text_contains_case_insensitive() ->
    Thread = empty_thread(),
    {ok, Value, _} = automate_bot_engine_operations:get_result(
                    #{ ?TYPE => ?COMMAND_STRING_CONTAINS
                     , ?ARGUMENTS => [ constant_val(<<"this is a TEst string">>)
                                     , constant_val(<<"teST">>)
                                     ]
                     }, Thread),
    ?assertMatch(true, Value).

text_contains_accent_insensitive() ->
    Thread = empty_thread(),
    {ok, Value, _} = automate_bot_engine_operations:get_result(
                    #{ ?TYPE => ?COMMAND_STRING_CONTAINS
                     , ?ARGUMENTS => [ constant_val(unicode:characters_to_binary("this an accent -åäö oaa- test string", utf8))
                                     , constant_val(unicode:characters_to_binary("-aao öäå-", utf8))
                                     ]
                     }, Thread),
    ?assertMatch(true, Value).

text_contains_several_map_to_same() ->
    Unicode = constant_val(unicode:characters_to_binary("Å Å ̊A ấ ą", utf8)), % Taken from http://www.macchiato.com/unicode/nfc-faq
    Ascii = constant_val(unicode:characters_to_binary("A A A A A", utf8)),
    Thread = empty_thread(),
    ?assertMatch({ok, true, _}, automate_bot_engine_operations:get_result(
                       #{ ?TYPE => ?COMMAND_STRING_CONTAINS
                        , ?ARGUMENTS => [ Unicode
                                        , Ascii
                                        ]
                        }, Thread)),
    ?assertMatch({ok, true, _}, automate_bot_engine_operations:get_result(
                                  #{ ?TYPE => ?COMMAND_STRING_CONTAINS
                                   , ?ARGUMENTS => [ Ascii
                                                   , Unicode
                                                   ]
                                   }, Thread)).

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
