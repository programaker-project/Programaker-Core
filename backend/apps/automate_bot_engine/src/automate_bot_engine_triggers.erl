-module(automate_bot_engine_triggers).

%% API
-export([ get_expected_signals/1
        , get_triggered_threads/2
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_expected_signals(#program_state{}) -> {ok, [atom()]}.
get_expected_signals(#program_state{triggers=Triggers}) ->
    {ok, get_expected_signals_from_triggers(Triggers)}.


-spec get_triggered_threads(#program_state{}, {atom(), any()}) -> {ok, [#program_thread{}]}.
get_triggered_threads(#program_state{triggers=Triggers, program_id=ProgramId}, Signal) ->
    { ok
    , lists:filtermap(fun(Thread) -> trigger_thread(Thread, Signal, ProgramId) end,
                      Triggers)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%% Expected signals
-spec get_expected_signals_from_triggers([#program_trigger{}]) -> [atom()].
get_expected_signals_from_triggers(Triggers) ->
    [get_expected_action_from_trigger(Trigger) || Trigger <- Triggers ].

-spec get_expected_action_from_trigger(#program_trigger{}) -> atom().
%% TODO: return a more specific monitor
get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?WAIT_FOR_MONITOR
                                                             }}) ->
    ?TRIGGERED_BY_MONITOR;
get_expected_action_from_trigger(_Trigger) ->
    ?SIGNAL_PROGRAM_TICK.

%%%% Thread creation
%%% Monitors
%% If any value is OK
-spec trigger_thread(#program_trigger{}, {atom(), any()}, binary()) -> 'false' | {'true', #program_thread{}}.
trigger_thread(#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                            , ?ARGUMENTS := #{ ?MONITOR_ID := MonitorId
                                                             , ?MONITOR_EXPECTED_VALUE := ?MONITOR_ANY_VALUE
                                                             }
                                            }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, _Value} },
               ProgramId) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    %% @TODO associate monitor value, like
    %% {ok, T1} = automate_bot_engine_variables:set_thread_value( Thread
    %%                                                          , ?TELEGRAM_BOT_NAME
    %%                                                          , BotName
    %%                                                          ),
    io:format("Thread: ~p~n", [Thread]),
    {true, Thread};

%% With matching value
trigger_thread(#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                            , ?ARGUMENTS := #{ ?MONITOR_ID := MonitorId
                                                             , ?MONITOR_EXPECTED_VALUE := Argument
                                                             }
                                            }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, MessageContent} },
               ProgramId) ->

    case automate_bot_engine_variables:resolve_argument(Argument) of
        {ok, MessageContent} ->
            Thread = #program_thread{ position=[1]
                                    , program=Program
                                    , global_memory=#{}
                                    , instruction_memory=#{}
                                    , program_id=ProgramId
                                    },

            %% @TODO associate monitor value, like
            %% {ok, T1} = automate_bot_engine_variables:set_thread_value( Thread
            %%                                                          , ?TELEGRAM_BOT_NAME
            %%                                                          , BotName
            %%                                                          ),
            io:format("Thread: ~p~n", [Thread]),
            {true, Thread};
        {ok, Found} ->
            io:format("No match. Expected “~p”, found “~p”~n", [MessageContent, Found]),
            false
    end;

%% If no match is found, don't create a thread
trigger_thread(Trigger, Message, ProgramId) ->
    notify_trigger_not_matched(Trigger, Message, ProgramId),
    false.

-ifdef(TEST).
notify_trigger_not_matched(Trigger, Message, ProgramId) ->
    io:format("Trigger (~p) not matching (~p) ~n", [Message, Trigger]).
-else.
notify_trigger_not_matched(_Trigger, Message, _ProgramId) ->
    io:format("No trigger (~p), ~n", [Message]).
-endif.
