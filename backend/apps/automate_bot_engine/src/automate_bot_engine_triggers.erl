-module(automate_bot_engine_triggers).

%% API
-export([ get_expected_signals/1
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_expected_signals_from_triggers([#program_trigger{}]) -> [atom()].
get_expected_signals_from_triggers(Triggers) ->
    [get_expected_action_from_trigger(Trigger) || Trigger <- Triggers ].

-spec get_expected_action_from_trigger(#program_trigger{}) -> atom().
get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                                             }}) ->
    ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED.
