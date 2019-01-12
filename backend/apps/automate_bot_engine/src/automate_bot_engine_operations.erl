-module(automate_bot_engine_operations).

%% API
-export([ get_expected_signals/1
        , run_threads/3
        , get_result/2
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

-spec get_block_result(map(), #program_thread{}) -> {ok, any()} | {error, not_found}.
get_result(Operation, Thread) ->
    get_block_result(Operation, Thread).

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

-spec get_instruction(#program_thread{}) -> {ok, map()} | {error, element_not_found}.
get_instruction(#program_thread{ position=[]}) ->
    {error, element_not_found};

get_instruction(#program_thread{ program=Program, position=Position}) ->
    case resolve_block_with_position(Program, Position) of
        {ok, Block} ->
            {ok, Block};
        {error, Reason} ->
            {error, Reason}
    end.

-spec resolve_block_with_position(list(), list()) -> {ok, map()} | {error, element_not_found}.
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
        { did_not_run, {new_state, NewThreadState} } ->
            partition_threads(T, State, Message, { Stopped, RanThisTick, [NewThreadState | DidNotRanThisTick] });
        { did_not_run, _Reason } ->
            partition_threads(T, State, Message, { Stopped, RanThisTick, [Thread | DidNotRanThisTick] });
        { ran_this_tick, NewThreadState } ->
            partition_threads(T, State, Message, { Stopped, [NewThreadState | RanThisTick], DidNotRanThisTick })
    end.

-spec run_thread(#program_thread{}, #program_state{}, {atom(), any()})
                -> {stopped, thread_finished} | {did_not_run, waiting}
                       | {did_not_run, {new_state, #program_thread{}}}
                       | {ran_this_tick, #program_thread{}}.
run_thread(Thread, State, Message ) ->
    case get_instruction(Thread) of
        {ok, Instruction} ->
            run_instruction(Instruction, Thread, State, Message);
        {error, element_not_found} ->
            {stopped, thread_finished}
    end.

run_instruction(#{ ?TYPE := ?COMMAND_SET_VARIABLE
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                    , ?VALUE := VariableName
                                    }
                                 , ValueArgument
                                 ]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArgument, Thread),
    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, VariableName, Value),
    {ran_this_tick, increment_position(NewThreadState)};


run_instruction(#{ ?TYPE := ?COMMAND_CHANGE_VARIABLE
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                    , ?VALUE := VariableName
                                    }
                                 , ValueArgument
                                 ]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Change} = automate_bot_engine_variables:resolve_argument(ValueArgument, Thread),
    NewValue = case automate_bot_engine_variables:get_program_variable(Thread, VariableName) of
                   {ok, PrevValue} ->
                       automate_bot_engine_values:add(PrevValue, Change);
                   {error, not_found} ->
                       Change
               end,
    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, VariableName, NewValue),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(#{ ?TYPE := ?COMMAND_REPEAT
                 , ?ARGUMENTS := [Argument]
                 }, Thread=#program_thread{ position=Position }, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {Times, Value} = case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
                         {ok, MemoryValue} ->
                             MemoryValue;
                         {error, not_found} ->
                             {ok, TimesStr} = automate_bot_engine_variables:resolve_argument(Argument, Thread),
                             LoopTimes = to_int(TimesStr),
                             {LoopTimes, 0}
            end,
    case Value < Times of
        true ->
            NextIteration = automate_bot_engine_variables:set_instruction_memory( Thread
                                                                                , {Times, Value + 1}
                                                                                ),
            {ran_this_tick, NextIteration#program_thread{ position=Position ++ [1] }};
        false ->
            NextIteration = automate_bot_engine_variables:unset_instruction_memory(Thread),
            {ran_this_tick, increment_position(NextIteration)}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_WAIT
                 , ?ARGUMENTS := [Argument]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Seconds} = automate_bot_engine_variables:resolve_argument(Argument, Thread),
    StartTime = case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
                    {ok, MemoryValue} ->
                        MemoryValue;
                    {error, not_found} ->
                        erlang:monotonic_time(millisecond)
            end,

    WaitFinished = StartTime + binary_to_integer(Seconds) * 1000 < erlang:monotonic_time(millisecond),
    case WaitFinished of
        true ->
            NextIteration = automate_bot_engine_variables:unset_instruction_memory(Thread),
            {ran_this_tick, increment_position(NextIteration)};
        false ->
            NextIteration = automate_bot_engine_variables:set_instruction_memory(Thread, StartTime),
            {did_not_run, {new_state, NextIteration}}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_ADD_TO_LIST
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                    , ?VALUE := ListName
                                    }
                                 , NewValueArg
                                 ]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, NewValue} = automate_bot_engine_variables:resolve_argument(NewValueArg, Thread),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, Value} ->
                          Value;
                      {error, not_found} ->
                          []
                  end,

    %% TODO (optimization) avoid using list++list
    ValueAfter = ValueBefore ++ [NewValue],

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(#{ ?TYPE := ?COMMAND_DELETE_OF_LIST
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                    , ?VALUE := ListName
                                    }
                                 , IndexValueArg
                                 ]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexValueArg, Thread),
    Index = to_int(IndexValue),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, Value} ->
                          Value;
                      {error, not_found} ->
                          []
                  end,

    ValueAfter = automate_bot_engine_naive_lists:remove_nth(ValueBefore, Index),

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(#{ ?TYPE := ?COMMAND_INSERT_AT_LIST
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                    , ?VALUE := ListName
                                    }
                                 , ValueArg
                                 , IndexArg
                                 ]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread),
    Index = to_int(IndexValue),
    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, ListOnDB} ->
                          ListOnDB;
                      {error, not_found} ->
                          []
                  end,

    ValueAfter = automate_bot_engine_naive_lists:insert_nth(ValueBefore, Index, Value),

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(#{ ?TYPE := ?COMMAND_REPLACE_VALUE_AT_INDEX
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                    , ?VALUE := ListName
                                    }
                                 , IndexArg
                                 , ValueArg
                                 ]
                 }, Thread, _State, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread),
    Index = to_int(IndexValue),
    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, ListOnDB} ->
                          ListOnDB;
                      {error, not_found} ->
                          []
                  end,

    ValueAfter = automate_bot_engine_naive_lists:replace_nth(ValueBefore, Index, Value),

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

%% TODO: Really call the services
run_instruction(#{ ?TYPE := ?COMMAND_CALL_SERVICE
                               , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                                , ?SERVICE_ACTION := Action
                                                , ?SERVICE_CALL_VALUES := Values
                                                }
                               }, Thread,
                #program_state{permissions=Permissions},
                {?SIGNAL_PROGRAM_TICK, _}) ->

    UserId = case Permissions of
                 undefined ->  % For simplification on test cases
                     undefined;
                 #program_permissions{ owner_user_id=OwnerUserId } ->
                     OwnerUserId
             end,
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, NewThread, _Value} = Module:call(Action, Values, Thread, UserId),
    {ran_this_tick, increment_position(NewThread)};

run_instruction(_Instruction, _Thread, _State, _Message) ->
    %% io:format("Unhandled instruction/msg: ~p/~p~n", [Instruction, Message]),
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


to_int(Value) when is_integer(Value) ->
    Value;
to_int(Value) when is_binary(Value) ->
    {IntValue, <<"">>} = string:to_integer(Value),
    IntValue.

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

%% Operators
get_block_result(#{ ?TYPE := ?COMMAND_JOIN
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            {ok, automate_bot_engine_values:add(FirstValue, SecondValue)};
        _ ->
            {error, not_found}
    end;

%% Variables
get_block_result(#{ ?TYPE := ?COMMAND_DATA_VARIABLE
                  , ?ARGUMENTS := [ Value
                                  ]
                  }, Thread) ->
    automate_bot_engine_variables:resolve_argument(Value, Thread);

%% List
get_block_result(#{ ?TYPE := ?COMMAND_ITEM_OF_LIST
                  , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                     , ?VALUE := ListName
                                     }
                                  , IndexArg
                                  ]
                  }, Thread) ->
    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread),
    Index = to_int(IndexValue),
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            automate_bot_engine_naive_lists:get_nth(List, Index);
        {error, not_found} ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_ITEMNUM_OF_LIST
                  , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                     , ?VALUE := ListName
                                     }
                                  , ValueArg
                                  ]
                  }, Thread) ->
    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread),
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            automate_bot_engine_naive_lists:get_item_num(List, Value);
        {error, not_found} ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_LENGTH_OF_LIST
                  , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                     , ?VALUE := ListName
                                     }
                                  ]
                  }, Thread) ->
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            automate_bot_engine_naive_lists:get_length(List);
        {error, not_found} ->
            []
    end;

get_block_result(#{ ?TYPE := ?COMMAND_LIST_CONTAINS_ITEM
                  , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                     , ?VALUE := ListName
                                     }
                                  , ValueArg
                                  ]
                  }, Thread) ->
    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread),
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            {ok, automate_bot_engine_naive_lists:contains(List, Value)};
        {error, not_found} ->
            {ok, false}
    end;

get_block_result(#{ ?TYPE := <<"monitor.retrieve.", MonitorId/binary>>
                  , ?ARGUMENTS := []
                  }, _Thread) ->
    case automate_monitor_engine:get_last_monitor_result(MonitorId) of
        {ok, Result} ->
            {ok, Result};
        {error, not_found} ->
            {ok, false}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_CALL_SERVICE
                 , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                  , ?SERVICE_ACTION := Action
                                  , ?SERVICE_CALL_VALUES := Values
                                  }
                 }, Thread=#program_thread{ program_id=PID }) ->

    {ok, #user_program_entry{ user_id=UserId }} = automate_storage:get_program_from_id(PID),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, _NewThread, Value} = Module:call(Action, Values, Thread, UserId),
    {ok, Value};

%% Fail
get_block_result(Block, _Thread) ->
    io:format("Result from: ~p~n", [Block]),
    erlang:error(bad_operation).
