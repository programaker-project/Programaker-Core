-module(automate_bot_engine_operations).

%% API
-export([ get_expected_signals/1
        , run_threads/3
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_expected_signals([#program_thread{}]) -> {ok, [atom()]}.
get_expected_signals(Threads) ->
    {ok, get_expected_signals_from_threads(Threads)}.

-spec run_threads([#program_thread{}], #program_state{}, {atom(), any()}) -> {ok, {[#program_thread{}], [#program_thread{}]}}.
run_threads(Threads, State, Message) ->
    { _Stopped, RanThisTick, DidNotRanThisTick } = run_and_split_threads(Threads, State, Message),
    {ok, {RanThisTick, DidNotRanThisTick}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_expected_signals_from_threads([#program_thread{}]) -> [atom()].
get_expected_signals_from_threads(Threads) ->
    [Signal ||
        Signal <- [get_expected_action_from_thread(Thread) || Thread <- Threads ],
        Signal =/= none
    ].

-spec get_expected_action_from_thread(#program_thread{}) -> atom().
get_expected_action_from_thread(Thread) ->
    case get_instruction(Thread) of
        {error, element_not_found} ->
            none;
        {ok, Operation} ->
            get_expected_action_from_operation(Operation)
    end.

get_expected_action_from_operation(_) ->
    ?SIGNAL_PROGRAM_TICK.

-spec get_instruction(#program_state{}) -> {ok, map()} | {error, element_not_found}.
get_instruction(#program_thread{ position=[]}) ->
    {error, element_not_found};

get_instruction(#program_thread{ program=Program, position=Position}) ->
    case resolve_block_with_position(Program, Position) of
        {ok, Block} ->
            {ok, Block};
        {error, Reason} ->
            {error, Reason}
    end.

-spec resolve_block_with_position(list(), list()) -> {ok, map()}.
resolve_block_with_position(Ast, [Position | _]) when Position > length(Ast) ->
    {error, element_not_found};

resolve_block_with_position(Ast, [Position | T]) ->
    resolve_subblock_with_position(lists:nth(Position, Ast), T).

-spec resolve_subblock_with_position(list(), list()) -> {ok, map()}.
resolve_subblock_with_position(Element, []) ->
    {ok, Element};

resolve_subblock_with_position(#{<<"contents">> := Contents}, [Position | _]) when Position > length(Contents) ->
    {error, element_not_found};

resolve_subblock_with_position(#{<<"contents">> := Contents}, [Position | T]) ->
    resolve_subblock_with_position(lists:nth(Position, Contents), T).


-spec run_and_split_threads([#program_thread{}], #program_state{}, { atom(), any() })
                           -> {[#program_thread{}], [#program_thread{}], [#program_thread{}]}.
run_and_split_threads(Threads, State, Message) ->
    partition_threads(Threads, State, Message, {[], [], []}).

-spec partition_threads([#program_thread{}], #program_state{}, { atom(), any() },
                        {[#program_thread{}], [#program_thread{}], [#program_thread{}]})
                       -> {[#program_thread{}], [#program_thread{}], [#program_thread{}]}.
partition_threads([], _State, _Message, { Stopped, RanThisTick, DidNotRanThisTick }) ->
    { Stopped, RanThisTick, DidNotRanThisTick };

partition_threads([Thread | T], State, Message, { Stopped, RanThisTick, DidNotRanThisTick }) ->
    case run_thread(Thread, State, Message) of
        { stopped, _Reason } ->
            partition_threads(T, State, Message, { [Thread | Stopped], RanThisTick, DidNotRanThisTick });
        { did_not_run, _Reason } ->
            partition_threads(T, State, Message, { Stopped, RanThisTick, [Thread | DidNotRanThisTick] });
        { ran_this_tick, NewThreadState } ->
            partition_threads(T, State, Message, { Stopped, [NewThreadState | RanThisTick], DidNotRanThisTick })
    end.

-spec run_thread(#program_thread{}, #program_state{}, {atom(), any()})
                -> {stopped, thread_finished} | {did_not_run, waiting} | {ran_this_tick, #program_thread{}}.
run_thread(Thread, State, Message ) ->
    case get_instruction(Thread) of
        {ok, Instruction} ->
            run_instruction(Instruction, Thread, State, Message);
        {error, element_not_found} ->
            {stopped, thread_finished}
    end.


run_instruction(#{ ?TYPE := ?COMMAND_TELEGRAM_ON_RECEIVED_COMMAND
                 , ?ARGUMENTS := [Argument]
                 }, Thread, _State, { ?SIGNAL_TELEGRAM_MESSAGE_RECEIVED, {_UserId, SignalContent} }) ->
    Expected = automate_bot_engine_variables:resolve_argument(Argument),
    case Expected of
        {ok, SignalContent} ->
            {ran_this_tick, increment_position(Thread)};
        {ok, _} ->
            {did_not_run, waiting};
        {error, _Reason} ->
            {did_not_run, waiting}
    end;


run_instruction(#{ ?TYPE := ?COMMAND_CHAT_SAY
                 , ?ARGUMENTS := [Argument]
                 }, Thread, _State, _Message) ->

    {ok, Message} = automate_bot_engine_variables:resolve_argument(Argument),
    case automate_bot_engine_variables:retrieve_thread_values(Thread, [ ?TELEGRAM_CHAT_ID
                                                                      , ?TELEGRAM_BOT_NAME
                                                                      ]) of
        {ok, [ChatId, BotName]} ->
            {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Message});
        {error, Reason} ->
            %% TODO report error to user
            io:format("Error: ~p~n", [Reason])
    end,
    {ran_this_tick, increment_position(Thread)};

run_instruction(Instruction, _Thread, _State, _Message) ->
    #{ ?TYPE := Type } = Instruction,
    io:format("Unhandled instruction, type: ~p~n", [Type]),
    {did_not_run, waiting}.


increment_position(Thread = #program_thread{position=Position}) ->
    IncrementedInnermost = increment_innermost(Position),
    BackToParent = back_to_parent(Position),
    FollowInSameLevelState = Thread#program_thread{position=IncrementedInnermost},
    BackToParentState = Thread#program_thread{position=BackToParent},
    case get_instruction(FollowInSameLevelState) of
        {ok, _} ->
            FollowInSameLevelState;
        {error, element_not_found} ->
            BackToParentState
    end.

back_to_parent([]) ->
    [1];
back_to_parent(List) ->
    case lists:reverse(List) of
        [_] ->  %% End reached, leave empty to remove on next iteration
            [];
        [_ | Tail] ->
            lists:reverse(Tail)
    end.

increment_innermost([]) ->
    [];
increment_innermost(List)->
    [Latest | Tail] = lists:reverse(List),
    lists:reverse([Latest + 1 | Tail]).
