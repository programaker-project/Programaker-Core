-module(automate_bot_engine_triggers).

%% API
-export([ get_expected_signals/1
        , get_triggered_threads/2
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_expected_signals(#program_state{}) -> {ok, [atom()]}.
get_expected_signals(#program_state{triggers=Triggers}) ->
    {ok, get_expected_signals_from_triggers(Triggers)}.


-spec get_triggered_threads(#program_state{}, {atom(), any()}) -> {ok, [#program_thread{}]}.
get_triggered_threads(Program=#program_state{triggers=Triggers}, Signal) ->
    { ok
    , lists:filtermap(fun(Thread) -> trigger_thread(Thread, Signal, Program) end,
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
                                                             , ?ARGUMENTS := #{ ?MONITOR_ID := MonitorId }
                                                             }}) ->

    automate_channel_engine:listen_channel(MonitorId, self()),
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
               ProgramState=#program_state{program_id=ProgramId}) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    {true, Thread};

%% With matching value
trigger_thread(#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                            , ?ARGUMENTS := #{ ?MONITOR_ID := MonitorId
                                                             , ?MONITOR_EXPECTED_VALUE := Argument
                                                             }
                                            }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }} },
               ProgramState=#program_state{program_id=ProgramId}) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    case automate_bot_engine_variables:resolve_argument(Argument, Thread, ProgramState) of
        {ok, MessageContent} ->

            {ok, NewThread} = automate_bot_engine_variables:set_last_monitor_value(
                                Thread, MonitorId, FullMessage),
            {true, NewThread};
        {ok, Found} ->
            %% io:format("No match. Expected “~p”, found “~p”~n", [MessageContent, Found]),
            false
    end;

%% If no match is found, don't create a thread
trigger_thread(Trigger, Message, ProgramState) ->
    notify_trigger_not_matched(Trigger, Message, ProgramState),
    false.

-ifdef(TEST).
notify_trigger_not_matched(_Trigger, { triggered_by_monitor
                                     , {<<"9b438e6e-1922-4253-a0bb-6af4435ba65f">>, _}},
                           _Program) ->
    ok;
notify_trigger_not_matched(_Trigger, {tick, _}, _Program) ->
    io:format(" *tick* ");
notify_trigger_not_matched(Trigger, Message, _Program) ->
    io:format("Trigger (~p) not matching (~p) ~n", [Message, Trigger]).
-else.
notify_trigger_not_matched(_Trigger, { triggered_by_monitor
                                     , {<<"9b438e6e-1922-4253-a0bb-6af4435ba65f">>, _}},
                           _Program) ->
    ok;
notify_trigger_not_matched(_Trigger, {tick, _}, _Program) ->
    ok;
%% notify_trigger_not_matched(Trigger, Message, _Program) ->
%%     io:format("Trigger (~p) not matching (~p) ~n", [Message, Trigger]).
notify_trigger_not_matched(_Trigger, _Message, _Program) ->
    ok.
-endif.
