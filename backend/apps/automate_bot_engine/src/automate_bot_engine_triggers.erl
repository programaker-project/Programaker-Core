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
get_triggered_threads(#program_state{triggers=Triggers}, Signal) ->
    { ok
    , lists:filtermap(fun(Thread) -> trigger_thread(Thread, Signal) end,
                      Triggers)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%% Expected signals
-spec get_expected_signals_from_triggers([#program_trigger{}]) -> [atom()].
get_expected_signals_from_triggers(Triggers) ->
    [get_expected_action_from_trigger(Trigger) || Trigger <- Triggers ].

-spec get_expected_action_from_trigger(#program_trigger{}) -> atom().
get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                                             }}) ->
    ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED.

%%%% Thread creation
%% Telegram
-spec trigger_thread(#program_trigger{}, {atom(), any()}) -> 'false' | {'true', #program_thread{}}.
trigger_thread(#program_trigger{ condition=#{ ?TYPE := ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                            , ?ARGUMENTS := [Argument]
                                            }
                               , subprogram=Program
                               },
               { ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED, {ChatId, Content, BotName} }) ->

    case automate_bot_engine_variables:resolve_argument(Argument) of
        {ok, Content} ->
            Thread = #program_thread{ position=[1]
                                    , program=Program
                                    , global_memory=#{}
                                    , variables=#{}
                                    },

            {ok, T1} = automate_bot_engine_variables:set_thread_value( Thread
                                                                     , ?TELEGRAM_BOT_NAME
                                                                     , BotName
                                                                     ),

            {ok, T2} = automate_bot_engine_variables:set_thread_value( T1
                                                                     , ?TELEGRAM_CHAT_ID
                                                                     , ChatId
                                                                     ),
            io:format("Thread: ~p~n", [Thread]),
            {true, T2};
        {ok, Found} ->
            io:format("No match. Expected “~p”, found “~p”~n", [Content, Found]),
            false;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            false
    end;

%% If no match is found, don't create a thread
trigger_thread(_Trigger, Message) ->
    io:format("No trigger (~p)~n", [Message]),
    false.
