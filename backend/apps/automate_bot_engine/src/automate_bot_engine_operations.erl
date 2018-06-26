-module(automate_bot_engine_operations).

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
get_expected_signals(#program_state{subprograms=Subprograms}) ->
    {ok, get_expected_signals_from_subprograms(Subprograms)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_expected_signals_from_threads([#program_thread{}]) -> [atom()].
get_expected_signals_from_threads(Threads) ->
    [get_expected_action_from_thread(Thread) || Thread <- Threads ].

-spec get_expected_action_from_thread(#program_thread{}) -> atom().
get_expected_action_from_thread(#program_thread{condition=#{ ?TYPE := ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                                                           }}) ->
    ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED.
