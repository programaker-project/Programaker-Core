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
-spec get_expected_signals(#program_state{}) -> {ok, [atom()]}.
get_expected_signals(#program_state{threads=Threads}) ->
    {ok, get_expected_signals_from_threads(Threads)}.

-spec run_threads([#program_thread{}], #program_state{}, {atom(), any()}) -> {ok, {[#program_thread{}], [#program_thread{}]}}.
run_threads(_Threads, _State, _Message) ->
    {ok, [], []}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_expected_signals_from_threads([#program_thread{}]) -> [atom()].
get_expected_signals_from_threads(Threads) ->
    [get_expected_action_from_thread(Thread) || Thread <- Threads ].

-spec get_expected_action_from_thread(#program_thread{}) -> atom().
get_expected_action_from_thread(Thread) ->
    {ok, Operation} = get_instruction(Thread),
    get_expected_action_from_operation(Operation).

get_expected_action_from_operation(_) ->
    ?SIGNAL_PROGRAM_TICK.

-spec get_instruction(#program_state{}) -> {ok, map()} | {error, element_not_found}.
get_instruction(#program_thread{ position=[]}) ->
    {error, not_initialized};

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
