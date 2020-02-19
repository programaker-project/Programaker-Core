-module(automate_bot_engine_operations).

%% API
-export([ get_expected_signals/1
        , run_thread/3
        , get_result/2
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

-define(UTILS, automate_bot_engine_utils).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_expected_signals([#program_thread{}]) -> {ok, [atom()]}.
get_expected_signals(Threads) ->
    {ok, get_expected_signals_from_threads(Threads)}.

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

-spec run_thread(#program_thread{}, {atom(), any()}, binary())
                -> {stopped, thread_finished} | {did_not_run, waiting}
                       | {did_not_run, {new_state, #program_thread{}}}
                       | {ran_this_tick, #program_thread{}}.
run_thread(Thread=#program_thread{program_id=ProgramId}, Message, ThreadId) ->
    case get_instruction(Thread) of
        {ok, Instruction} ->
            try
                run_instruction(Instruction, Thread, Message)
            of
                Result ->
                    Result
            catch ErrorNS:Error:StackTrace ->
                    io:fwrite("[ERROR][Thread][ProgId=~p,ThreadId=~p] Critical error: ~p~n~p~n",
                              [ProgramId, ThreadId, {ErrorNS, Error}, StackTrace]),

                    UserId = case automate_storage:get_program_owner(ProgramId) of
                                 {ok, OwnerId} ->
                                     OwnerId;
                                 {error, Reason} ->
                                     io:fwrite("[Double ERROR][ThreadId=~p] Now owner found: ~p~n",
                                               [ThreadId, Reason]),
                                     none
                             end,

                    {EventData, EventMessage, FailedBlockId} =
                        case Error of
                            #program_error{ error=#variable_not_set{variable_name=VariableName}
                                          , block_id=BlockId
                                          } ->
                                { Error
                                , binary:list_to_bin(
                                    lists:flatten(io_lib:format("Variable '~s' not set", [VariableName])))
                                , BlockId
                                };

                            #program_error{ error=#list_not_set{list_name=ListName}
                                          , block_id=BlockId
                                          } ->
                                { Error
                                , binary:list_to_bin(
                                    lists:flatten(io_lib:format("List '~s' not set", [ListName])))
                                , BlockId
                                };

                            #program_error{error=#index_not_in_list{ list_name=ListName
                                                                   , index=Index
                                                                   , max=MaxIndex
                                                                   }
                                          , block_id=BlockId
                                          } ->
                                { Error
                                , binary:list_to_bin(
                                    lists:flatten(io_lib:format("Cannot access position ~s on list '~s'. Only ~s elements",
                                                                [ListName, Index, MaxIndex])))
                                , BlockId
                                };

                            #program_error{error=#invalid_list_index_type{ list_name=ListName
                                                                         , index=Index
                                                                         }
                                          , block_id=BlockId
                                          } ->
                                { Error
                                , binary:list_to_bin(
                                    lists:flatten(io_lib:format("Trying to access non valid position list '~s'. "
                                                                "Position must be a non-negative number. Found '~s'.",
                                                                [ListName, Index])))
                                , BlockId
                                };
                            _ ->
                                %% Although this might be extracted from the thread's position
                                {Error, <<"Unknown error">>, undefined}
                        end,

                    automate_logging:log_program_error(#user_program_log_entry{ program_id=ProgramId
                                                                              , thread_id=ThreadId
                                                                              , user_id=UserId
                                                                              , block_id=FailedBlockId
                                                                              , event_data=EventData
                                                                              , event_time=erlang:system_time(millisecond)
                                                                              , event_message=EventMessage
                                                                              , severity=error
                                                                              , exception_data={ErrorNS,Error,StackTrace}
                                                                              }),
                    {stopped, {ErrorNS, Error}}  %% Critical errors trigger a stop
            end;
        {error, element_not_found} ->
            {stopped, thread_finished}
    end.

run_instruction(#{ ?TYPE := ?COMMAND_SET_VARIABLE
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                    , ?VALUE := VariableName
                                    }
                                 , ValueArgument
                                 ]
                 }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArgument, Thread),
    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, VariableName, Value),
    {ran_this_tick, increment_position(NewThreadState)};


run_instruction(Op=#{ ?TYPE := ?COMMAND_CHANGE_VARIABLE
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                       , ?VALUE := VariableName
                                       }
                                    , ValueArgument
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Change} = automate_bot_engine_variables:resolve_argument(ValueArgument, Thread),
    {ok, NewValue} = case automate_bot_engine_variables:get_program_variable(Thread, VariableName) of
                         {ok, PrevValue} ->
                             automate_bot_engine_values:add(PrevValue, Change);
                         {error, not_found} ->
                             throw(#program_error{ error=#variable_not_set{ variable_name=VariableName }
                                                 , block_id=?UTILS:get_block_id(Op)
                                                 })
                     end,
    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, VariableName, NewValue),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(#{ ?TYPE := ?COMMAND_REPEAT
                 , ?ARGUMENTS := [Argument]
                 }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

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

run_instruction(#{ ?TYPE := ?COMMAND_REPEAT_UNTIL
                 , ?ARGUMENTS := [Argument]
                 }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Value} = automate_bot_engine_variables:resolve_argument(Argument, Thread),
    case Value of
        false ->
            {ran_this_tick, Thread#program_thread{ position=Position ++ [1] }};
        _ ->
            {ran_this_tick, increment_position(Thread)}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_IF
                 , ?ARGUMENTS := [Argument]
                 }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
        {ok, _} ->
            NextIteration = automate_bot_engine_variables:unset_instruction_memory(Thread),
            {ran_this_tick, increment_position(NextIteration)};
        {error, not_found} ->
            {ok, Value} = automate_bot_engine_variables:resolve_argument(
                            Argument, Thread),

            case Value of
                false -> %% Not matching, skipping
                    {ran_this_tick, increment_position(Thread)};
                _ -> %% Matching, going in
                    NextIteration = automate_bot_engine_variables:set_instruction_memory(
                                      Thread, {already_run, true}),
                    {ran_this_tick, NextIteration#program_thread{ position=Position ++ [1] }}

            end
    end;

run_instruction(#{ ?TYPE := ?COMMAND_IF_ELSE
                 , ?ARGUMENTS := [Argument]
                 }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
        {ok, _} ->
            NextIteration = automate_bot_engine_variables:unset_instruction_memory(Thread),
            {ran_this_tick, increment_position(NextIteration)};
        {error, not_found} ->
            NextIteration = automate_bot_engine_variables:set_instruction_memory(
                              Thread, {already_run, true}),
            {ok, Value} = automate_bot_engine_variables:resolve_argument(Argument, NextIteration),
            case Value of
                false -> %% Not matching, going for else
                    {ran_this_tick, NextIteration#program_thread{ position=Position ++ [2, 1] }};
                _ -> %% Matching, going for if
                    {ran_this_tick, NextIteration#program_thread{ position=Position ++ [1, 1] }}
            end
    end;

run_instruction(#{ ?TYPE := ?COMMAND_WAIT_UNTIL
                 , ?ARGUMENTS := [Argument]
                 }, Thread=#program_thread{}, _) ->

    {ok, Value} = automate_bot_engine_variables:resolve_argument(Argument, Thread),
    case Value of
        false ->
            {did_not_run, Thread};
        _ ->
            {ran_this_tick, increment_position(Thread)}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_WAIT
                 , ?ARGUMENTS := [Argument]
                 }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

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

run_instruction(Op=#{ ?TYPE := ?COMMAND_ADD_TO_LIST
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                    , ?VALUE := ListName
                                    }
                                 , NewValueArg
                                 ]
                 }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, NewValue} = automate_bot_engine_variables:resolve_argument(NewValueArg, Thread),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, Value} ->
                          Value;
                      {error, not_found} ->
                          throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                              , block_id=?UTILS:get_block_id(Op)
                                              })
                  end,

    %% TODO (optimization) avoid using list++list
    ValueAfter = ValueBefore ++ [NewValue],

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_DELETE_OF_LIST
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , IndexValueArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexValueArg, Thread),
    Index = to_int(IndexValue),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, Value} ->
                          Value;
                      {error, not_found} ->
                          throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                              , block_id=?UTILS:get_block_id(Op)
                                              })
                  end,

    ValueAfter = automate_bot_engine_naive_lists:remove_nth(ValueBefore, Index),

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_INSERT_AT_LIST
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , ValueArg
                                    , IndexArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread),
    Index = to_int(IndexValue),
    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, ListOnDB} ->
                          ListOnDB;
                      {error, not_found} ->
                          throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                              , block_id=?UTILS:get_block_id(Op)
                                              })
                  end,

    ValueAfter = automate_bot_engine_naive_lists:insert_nth(ValueBefore, Index, Value),

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_REPLACE_VALUE_AT_INDEX
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , IndexArg
                                    , ValueArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread),
    Index = to_int(IndexValue),
    {ok, Value} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
                      {ok, ListOnDB} ->
                          ListOnDB;
                      {error, not_found} ->
                          throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                              , block_id=?UTILS:get_block_id(Op)
                                              })
                  end,

    ValueAfter = automate_bot_engine_naive_lists:replace_nth(ValueBefore, Index, Value),

    {ok, NewThreadState } = automate_bot_engine_variables:set_program_variable(Thread, ListName, ValueAfter),
    {ran_this_tick, increment_position(NewThreadState)};

run_instruction(#{ ?TYPE := ?COMMAND_CALL_SERVICE
                 , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                  , ?SERVICE_ACTION := Action
                                  , ?SERVICE_CALL_VALUES := Arguments
                                  }
                 }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    Values = lists:map(fun (Arg) ->
                               {ok, Value} = automate_bot_engine_variables:resolve_argument(Arg, Thread),
                               Value
                       end, Arguments),

    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, NewThread, _Value} = automate_service_registry_query:call(Module, Action, Values, Thread, UserId),
    {ran_this_tick, increment_position(NewThread)};

run_instruction(Operation=#{ ?TYPE := <<"services.", ServiceCall/binary>>
                           , ?ARGUMENTS := Arguments
                           }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    SaveTo = get_save_to(Operation),
    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    ReadArguments = remove_save_to(Arguments, SaveTo),
    Values = lists:map(fun (Arg) ->
                               {ok, Value} = automate_bot_engine_variables:resolve_argument(Arg, Thread),
                               Value
                       end, ReadArguments),

    [ServiceId, Action] = binary:split(ServiceCall, <<".">>),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, NewThread, Value} = automate_service_registry_query:call(Module, Action, Values, Thread, UserId),

    {ok, SavedThread} = case SaveTo of
                            { index, Index } ->
                                #{ <<"value">> := VariableName
                                 } = lists:nth(Index, Arguments),
                                automate_bot_engine_variables:set_program_variable(
                                  Thread,
                                  %% Note that erlang is 1-indexed, protocol is 0-indexed
                                  VariableName,
                                  Value);
                            _ ->
                                {ok, NewThread}
                        end,
    {ran_this_tick, increment_position(SavedThread)};

run_instruction(#{ ?TYPE := ?MATCH_TEMPLATE_STATEMENT
                 , ?ARGUMENTS := [#{ ?TYPE := ?TEMPLATE_NAME_TYPE
                                   , ?VALUE := TemplateId
                                   }
                                 , Input
                                 ]
                 }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    {ok, InputValue} = automate_bot_engine_variables:resolve_argument(Input, Thread),

    case automate_template_engine:match(UserId, Thread, TemplateId, InputValue) of
        {ok, NewThread, _Value} ->
            {ran_this_tick, increment_position(NewThread)};
        {error, not_found} ->
            {ran_this_tick, finish_thread(Thread)}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_CUSTOM_SIGNAL
                      , ?ARGUMENTS := [ SignalIdVal
                                      , SignalDataVal
                                      ]
                      }, Thread=#program_thread{ program_id=_ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, ChannelId } = automate_bot_engine_variables:resolve_argument(SignalIdVal, Thread),
    {ok, SignalData } = automate_bot_engine_variables:resolve_argument(SignalDataVal, Thread),

    ok = automate_channel_engine:send_to_channel(ChannelId, SignalData),

    {ran_this_tick, increment_position(Thread)};

run_instruction(#{ ?TYPE := Instruction }, _Thread, Message) ->
    io:format("Unhandled instruction/msg: ~p/~p~n", [Instruction, Message]),
    {did_not_run, waiting};

run_instruction(#{ <<"contents">> := _Content }, Thread, _Message) ->
    %% Finished code block
    {ran_this_tick, increment_position(Thread)}.

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

finish_thread(Thread = #program_thread{}) ->
    Thread#program_thread{position=[]}.

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

%%%% Operators
%% String operators
get_block_result(#{ ?TYPE := ?COMMAND_JOIN
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),

    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:join(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;
get_block_result(#{ ?TYPE := ?COMMAND_JSON
                  , ?ARGUMENTS := [ KeyReference
                                  , MapReference
                                  ]
                  }, Thread) ->
    KeyResult = automate_bot_engine_variables:resolve_argument(KeyReference, Thread),
    MapResult = automate_bot_engine_variables:resolve_argument(MapReference, Thread),

    case [KeyResult, MapResult] of
        [{ok, KeyValue}, {ok, MapValue}] ->
            automate_bot_engine_values:get_value_by_key(KeyValue, MapValue);
        _ ->
            {error, not_found}
    end;



%% Templates
get_block_result(#{ ?TYPE := ?MATCH_TEMPLATE_CHECK
                  , ?ARGUMENTS := [#{ ?TYPE := ?TEMPLATE_NAME_TYPE
                                    , ?VALUE := TemplateId
                                    }
                                  , Input
                                  ]
                  }, Thread=#program_thread{ program_id=ProgramId }) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    {ok, InputValue} = automate_bot_engine_variables:resolve_argument(Input, Thread),

    case automate_template_engine:match(UserId, Thread, TemplateId, InputValue) of
        {ok, NewThread, _Value} ->
            {ok, true};
        {error, not_found} ->
            {ok, false}
    end;

%% Numeric operators
get_block_result(#{ ?TYPE := ?COMMAND_ADD
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:add(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_SUBTRACT
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:subtract(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_MULTIPLY
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:multiply(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_DIVIDE
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:divide(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

%% Comparations
get_block_result(#{ ?TYPE := ?COMMAND_LESS_THAN
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:is_less_than(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_GREATER_THAN
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:is_greater_than(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_EQUALS
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, FirstValue}, {ok, SecondValue}] ->
            automate_bot_engine_values:is_equal_to(FirstValue, SecondValue);
        _ ->
            {error, not_found}
    end;

%% Boolean operations
get_block_result(#{ ?TYPE := ?COMMAND_AND
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, true}, {ok, true}] ->
            {ok, true};
        [_, _] ->
            {ok, false};
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_OR
                  , ?ARGUMENTS := [ First
                                  , Second
                                  ]
                  }, Thread) ->
    FirstResult = automate_bot_engine_variables:resolve_argument(First, Thread),
    SecondResult = automate_bot_engine_variables:resolve_argument(Second, Thread),
    case [FirstResult, SecondResult] of
        [{ok, true}, _] ->
            {ok, true};
        [_, {ok, true}] ->
            {ok, true};
        [_, _] ->
            {ok, false};
        _ ->
            {error, not_found}
    end;

get_block_result(#{ ?TYPE := ?COMMAND_NOT
                  , ?ARGUMENTS := [ Value
                                  ]
                  }, Thread) ->
    Result = automate_bot_engine_variables:resolve_argument(Value, Thread),
    case Result of
        {ok, false} ->
            {ok, true};
        {ok, true} ->
            {ok, false};
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
get_block_result(Op=#{ ?TYPE := ?COMMAND_ITEM_OF_LIST
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
            case automate_bot_engine_naive_lists:get_nth(List, Index) of
                {ok, Value } ->
                    {ok, Value};
                {error, not_found} ->
                    throw(#program_error{ error=#index_not_in_list{list_name=ListName, index=Index, max=length(List)}
                                        , block_id=?UTILS:get_block_id(Op)
                                        });
                {error, invalid_list_index_type} ->
                    throw(#program_error{ error=#invalid_list_index_type{list_name=ListName, index=Index}
                                        , block_id=?UTILS:get_block_id(Op)
                                        })
            end;
        {error, not_found} ->
            throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_ITEMNUM_OF_LIST
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
            throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_LENGTH_OF_LIST
                     , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                        , ?VALUE := ListName
                                        }
                                     ]
                     }, Thread) ->
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            automate_bot_engine_naive_lists:get_length(List);
        {error, not_found} ->
            throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_LIST_CONTAINS_ITEM
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
            throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(#{ ?TYPE := <<"monitor.retrieve.", MonitorId/binary>>
                  , ?ARGUMENTS := []
                  }, _Thread) ->
    case automate_monitor_engine:get_last_monitor_result(MonitorId) of
        {ok, Result} ->
            {ok, Result}
    end;

get_block_result(#{ ?TYPE := <<"services.", ServiceCall/binary>>
                  , ?ARGUMENTS := Arguments
                  }, Thread=#program_thread{ program_id=ProgramId }) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    Values = lists:map(fun (Arg) ->
                               {ok, Value} = automate_bot_engine_variables:resolve_argument(Arg, Thread),
                               Value
                       end, Arguments),

    [ServiceId, Action] = binary:split(ServiceCall, <<".">>),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, _NewThread, Value} = automate_service_registry_query:call(Module, Action, Values, Thread, UserId),
    {ok, Value};

get_block_result(#{ ?TYPE := ?COMMAND_CALL_SERVICE
                  , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                   , ?SERVICE_ACTION := Action
                                   , ?SERVICE_CALL_VALUES := Values
                                   }
                  }, Thread=#program_thread{ program_id=PID }) ->

    {ok, #user_program_entry{ user_id=UserId }} = automate_storage:get_program_from_id(PID),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, _NewThread, Value} = automate_service_registry_query:call(Module, Action, Values, Thread, UserId),
    {ok, Value};

%% Fail
get_block_result(Block, _Thread) ->
    io:format("Result from: ~p~n", [Block]),
    erlang:error(bad_operation).


get_save_to(#{ <<"save_to">> := #{ <<"type">> := <<"argument">>
                                 , <<"index">> := Index
                                 } }) ->
    { index, Index + 1 }; %% erlang is 1-indexed, protocol is 0-indexed
get_save_to(_) ->
    none.

remove_save_to(Arguments, none) ->
    Arguments;
remove_save_to(Arguments, {index, Index}) ->
    automate_bot_engine_naive_lists:remove_nth(Arguments, Index).
