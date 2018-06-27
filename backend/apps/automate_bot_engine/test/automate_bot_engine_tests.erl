%%% @doc
%%% Automate bot engine tests.
%%% @end

-module(automate_bot_engine_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").

%% Test data
-include("single_line_program.hrl").

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
    %% NodeName = node(),

    %% %% Use a custom node name to avoid overwriting the actual databases
    %% net_kernel:start([?MODULE, shortnames]),
    %% hivemind_app:install(),
    %% {ok, Pid} = hivemind_app:start(),

    %% {NodeName, Pid}.
    ok.

%% @doc App infrastructure teardown.
%% @end
%% stop({NodeName, Pid}) ->
stop(ok) ->
    %% hivemind_app:stop(Pid),
    %% ok = mnesia:delete_schema(?TEST_NODES),

    %% %% Restore the original node name
    %% net_kernel:start([NodeName, shortnames]).
    ok.

tests(_SetupResult) ->
    [ {"[Bot runner][Initialization] Single line program initialization", fun single_line_program_initialization/0}
    , {"[Bot runner][Signals] Wait for telegram command signal", fun wait_for_telegram_command_signal/0}
    , {"[Bot runner][Resolution] Constant argument resolution", fun constant_argument_resolution/0}
    , {"[Bot runner][Triggers] Trigger thread with telegram command", fun trigger_thread_with_telegram_command/0}
    , {"[Bot runner][Threads] Run a thread a single tick", fun run_thread_single_tick/0}
    ].


%%%% Bot runner
%% Initialization
single_line_program_initialization() ->
    Program  = ?SINGLE_LINE_PROGRAM,
    Expected = ?SINGLE_LINE_PROGRAM_INITIALIZATION,

    {ok, Expected} = automate_bot_engine_program_decoder:initialize_program(?SINGLE_LINE_PROGRAM_ID, Program).

%% Signals
wait_for_telegram_command_signal() ->
    Program = #program_state{ triggers=[#program_trigger{ condition=#{ ?TYPE => ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                                                     }
                                                        }]},
    Expected = [?SIGNAL_TELEGRAM_MESSAGE_RECEIVED],

    {ok, Expected} = automate_bot_engine_triggers:get_expected_signals(Program).

%% Argument resolution
constant_argument_resolution() ->
    Value = example,
    {ok, Value} = automate_bot_engine_variables:resolve_argument(#{ ?TYPE => ?VARIABLE_CONSTANT
                                                                  , ?VALUE => Value
                                                                  }).

%% Threads
trigger_thread_with_telegram_command() ->
    Program = #program_state{ triggers=[#program_trigger{ condition=#{ ?TYPE => ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                                                     , ?ARGUMENTS => [#{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => example
                                                                                       }
                                                                                     ]
                                                                     }
                                                        , subprogram=[#{ ?TYPE => example }]
                                                        }]},
    {ok, [Thread]} = automate_bot_engine_triggers:get_triggered_threads(Program, { ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED
                                                                                 , { 0123, example, botname }}),
    #program_thread{ position=[1], program=[#{ ?TYPE := example }] } = Thread.

run_thread_single_tick() ->
    WaitForTelegramCommandInstruction = #{ ?TYPE => ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                         , ?ARGUMENTS => [#{ ?TYPE => ?VARIABLE_CONSTANT
                                                           , ?VALUE => example
                                                           }
                                                         ]
                                         },
    ChatSayInstruction = #{ ?TYPE => ?COMMAND_CHAT_SAY
                                         , ?ARGUMENTS => [#{ ?TYPE => ?VARIABLE_CONSTANT
                                                           , ?VALUE => answer
                                                           }
                                                         ]
                                         },
    TelegramCommandSignal = { ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED
                            , { 0123, example, botname }},

    Program = #program_state{ triggers=[#program_trigger{ condition=WaitForTelegramCommandInstruction
                                                        , subprogram=[ChatSayInstruction]
                                                        }]},

    {ok, [Thread]} = automate_bot_engine_triggers:get_triggered_threads(Program, TelegramCommandSignal),

    %% Unexpected signal, does not run
    #program_thread{ position=[1], program=[ChatSayInstruction] } = Thread,
    {ok, {Ran1, NotRun1}} = automate_bot_engine_operations:run_threads([Thread], Program#program_state{ threads=[Thread] },
                                                                       TelegramCommandSignal),
    [] = Ran1,
    [#program_thread{position=[1]}] = NotRun1,

    %% Expected signal, does run
    {ok, {Ran2, NotRun2}} = automate_bot_engine_operations:run_threads([Thread], Program#program_state{ threads=[Thread] },
                                                                       {?SIGNAL_PROGRAM_TICK, none}),
    [#program_thread{position=[]}] = Ran2,
    [] = NotRun2.
