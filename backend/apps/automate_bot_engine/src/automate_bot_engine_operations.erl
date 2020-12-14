-module(automate_bot_engine_operations).

%% API
-export([ get_expected_signals/1
        , run_thread/3
        , get_result/2
        ]).

-ifdef(TEST).
-export([ run_instruction/3
        ]).
-endif.

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

-define(UTILS, automate_bot_engine_utils).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_expected_signals([#program_thread{}]) -> {ok, [atom()]}.
get_expected_signals(Threads) ->
    {ok, get_expected_signals_from_threads(Threads)}.

-spec get_result(map(), #program_thread{}) -> {ok, any(), #program_thread{}} | {error, not_found}.
get_result(Operation, Thread) ->
    run_getter_block(Operation, Thread).

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
            get_expected_action_from_operation(Operation, Thread)
    end.

get_expected_action_from_operation(#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_BLOCK
                                                       , ?VALUE := [ Listened=#{ ?TYPE := <<"services.", MonitorPath/binary>>
                                                                               }
                                                                   ]
                                                       }
                                                    ]
                                    }, #program_thread{ program_id=ProgramId }) ->
    [ServiceId, MonitorKey] = binary:split(MonitorPath, <<".">>),

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    Args = case Listened of
               #{ ?ARGUMENTS := Arguments } ->
                   Arguments;
               _ ->
                   []
           end,
    case ?UTILS:get_block_key_subkey(Args) of
        { key_and_subkey, Key, SubKey } ->
            automate_service_registry_query:listen_service(ServiceId, UserId, { Key, SubKey });
        { key, Key } ->
            automate_service_registry_query:listen_service(ServiceId, UserId, { Key, undefined });
        { not_found } ->
            automate_service_registry_query:listen_service(ServiceId, UserId, { MonitorKey, undefined })
    end,

    ?TRIGGERED_BY_MONITOR;

get_expected_action_from_operation(#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                                       , ?VALUE := _VarName
                                                       }
                                                    ]
                                    }, _Thread) ->
    %% TODO: Have channel to send variable updates
    ?SIGNAL_PROGRAM_TICK;

get_expected_action_from_operation(#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_BLOCK
                                                       , ?VALUE := [ #{ ?TYPE := ?WAIT_FOR_MONITOR
                                                                      , ?ARGUMENTS := MonitorArgs=#{ ?FROM_SERVICE := ServiceId }
                                                                      }
                                                                   ]
                                                       }
                                                    ]
                                    }, #program_thread{ program_id=ProgramId }) ->

    {ok, Owner} = automate_storage:get_program_owner(ProgramId),

    Key = case MonitorArgs of
              #{ <<"key">> := MonKey } ->
                  MonKey;
              _ ->
                  undefined
    end,
    automate_service_registry_query:listen_service(ServiceId, Owner, {Key, undefined}),
    ?TRIGGERED_BY_MONITOR;


get_expected_action_from_operation(Op=#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                                       }, _Thread) ->
    automate_logging:log_platform(error,
                                  io_lib:format("Cannot find appropriate channel to hear for op: ~p", [Op])),
    ?TRIGGERED_BY_MONITOR;

get_expected_action_from_operation(_Op, _Thread) ->
    %% Just wait for next TICK
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
                    case {Result, Instruction} of
                        { {ran_this_tick, Thread2}
                        , #{ ?BLOCK_ID := BlockId
                           , ?REPORT_STATE := true
                           }
                        } ->
                            {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),

                            Value = case automate_bot_engine_variables:retrieve_instruction_memory(Thread2, BlockId) of
                                        {error, not_found} -> none;
                                        {ok, BlockMem} -> BlockMem
                                    end,

                            %% Trigger element update
                            ok = automate_channel_engine:send_to_channel(ChannelId, #{ <<"key">> => block_run_events
                                                                                     , <<"subkey">> => BlockId
                                                                                     , <<"value">> => Value
                                                                                     } );
                        _ -> ok
                    end,
                    Result
            catch ErrorNS:Error:StackTrace ->
                    io:fwrite("[ERROR][Thread][ProgId=~p,ThreadId=~p] Critical error: ~0tp~n~0tp~n",
                              [ProgramId, ThreadId, {ErrorNS, Error}, StackTrace]),

                    UserId = case automate_storage:get_program_owner(ProgramId) of
                                 {ok, OwnerId} ->
                                     OwnerId;
                                 {error, Reason} ->
                                     io:fwrite("[Double ERROR][ThreadId=~p] Now owner found: ~0tp~n",
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
                                                                [Index, ListName, MaxIndex])))
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

                            #program_error{error=#unknown_operation{}
                                          , block_id=BlockId
                                          } ->
                                { Error
                                , binary:list_to_bin(
                                    lists:flatten(io_lib:format("Unknown operation found! Please, report this to the administrator",
                                                               [])))
                                , BlockId
                                };
                            _ ->
                                %% Although this might be extracted from the thread's position
                                {Error, <<"Unknown error">>, none}
                        end,

                    automate_logging:log_program_error(#user_program_log_entry{ program_id=ProgramId
                                                                              , thread_id=ThreadId
                                                                              , owner=UserId
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

run_instruction(Op=#{ ?TYPE := ?COMMAND_SET_VARIABLE
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                       , ?VALUE := VariableName
                                       }
                                    , ValueArgument
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(ValueArgument, Thread, Op),
    {ok, Thread3 } = automate_bot_engine_variables:set_program_variable(Thread2, VariableName, Value),
    ok = notify_variable_update(VariableName, Thread3),
    {ran_this_tick, increment_position(Thread3)};


run_instruction(Op=#{ ?TYPE := ?COMMAND_CHANGE_VARIABLE
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                       , ?VALUE := VariableName
                                       }
                                    , ValueArgument
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Change, Thread2} = automate_bot_engine_variables:resolve_argument(ValueArgument, Thread, Op),
    {ok, NewValue} = case automate_bot_engine_variables:get_program_variable(Thread2, VariableName) of
                         {ok, PrevValue} ->
                             automate_bot_engine_values:add(PrevValue, Change);
                         {error, not_found} ->
                             throw(#program_error{ error=#variable_not_set{ variable_name=VariableName }
                                                 , block_id=?UTILS:get_block_id(Op)
                                                 })
                     end,
    {ok, Thread3 } = automate_bot_engine_variables:set_program_variable(Thread2, VariableName, NewValue),
    ok = notify_variable_update(VariableName, Thread3),
    {ran_this_tick, increment_position(Thread3)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_REPEAT
                    , ?ARGUMENTS := [Argument]
                    }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    {Times, Value, Thread3} = case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
                         {ok, {WasTimes, WasValue}} ->
                             {WasTimes, WasValue, Thread};
                         {error, not_found} ->
                             {ok, TimesStr, Thread2} = automate_bot_engine_variables:resolve_argument(Argument, Thread, Op),
                             LoopTimes = to_int(TimesStr),
                             {LoopTimes, 0, Thread2}
                     end,
    case Value < Times of
        true ->
            Thread4 = automate_bot_engine_variables:set_instruction_memory( Thread3
                                                                          , {Times, Value + 1}
                                                                          ),
            {ran_this_tick, Thread4#program_thread{ position=Position ++ [1] }};
        false ->
            Thread4 = automate_bot_engine_variables:unset_instruction_memory(Thread3),
            {ran_this_tick, increment_position(Thread4)}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_REPEAT_UNTIL
                    , ?ARGUMENTS := [Argument]
                    }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(Argument, Thread, Op),
    case Value of
        false ->
            {ran_this_tick, Thread2#program_thread{ position=Position ++ [1] }};
        _ ->
            {ran_this_tick, increment_position(Thread2)}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_IF
                    , ?ARGUMENTS := [Argument]
                    }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
        {ok, _} ->
            Thread2 = automate_bot_engine_variables:unset_instruction_memory(Thread),
            {ran_this_tick, increment_position(Thread2)};
        {error, not_found} ->
            {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(
                                     Argument, Thread, Op),

            case Value of
                false -> %% Not matching, skipping
                    {ran_this_tick, increment_position(Thread2)};
                _ -> %% Matching, going in
                    Thread3 = automate_bot_engine_variables:set_instruction_memory(
                                Thread2, {already_run, true}),
                    {ran_this_tick, Thread3#program_thread{ position=Position ++ [1] }}
            end
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_IF_ELSE
                    , ?ARGUMENTS := [Argument]
                    }, Thread=#program_thread{ position=Position }, {?SIGNAL_PROGRAM_TICK, _}) ->

    case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
        {ok, _} ->
            Thread2 = automate_bot_engine_variables:unset_instruction_memory(Thread),
            {ran_this_tick, increment_position(Thread2)};
        {error, not_found} ->
            Thread2 = automate_bot_engine_variables:set_instruction_memory(
                              Thread, {already_run, true}),
            {ok, Value, Thread3} = automate_bot_engine_variables:resolve_argument(Argument, Thread2, Op),
            case Value of
                false -> %% Not matching, going for else
                    {ran_this_tick, Thread3#program_thread{ position=Position ++ [2, 1] }};
                _ -> %% Matching, going for if
                    {ran_this_tick, Thread3#program_thread{ position=Position ++ [1, 1] }}
            end
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_WAIT_UNTIL
                    , ?ARGUMENTS := [Argument]
                    }, Thread=#program_thread{}, _) ->

    {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(Argument, Thread, Op),
    case Value of
        false ->
            {did_not_run, waiting};
        _ ->
            {ran_this_tick, increment_position(Thread2)}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_WAIT
                    , ?ARGUMENTS := [Argument]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(Argument, Thread, Op),
    StartTime = case automate_bot_engine_variables:retrieve_instruction_memory(Thread2) of
                    {ok, MemoryValue} ->
                        MemoryValue;
                    {error, not_found} ->
                        erlang:monotonic_time(millisecond)
                end,

    MsToWait = case Value of
                     B when is_binary(B) ->
                         binary_to_integer(B) * 1000;
                     N when is_number(N) ->
                         N * 1000
                 end,

    WaitFinished = StartTime + MsToWait < erlang:monotonic_time(millisecond),
    case WaitFinished of
        true ->
            Thread3 = automate_bot_engine_variables:unset_instruction_memory(Thread2),
            {ran_this_tick, increment_position(Thread3)};
        false ->
            Thread3 = automate_bot_engine_variables:set_instruction_memory(Thread2, StartTime),
            {did_not_run, {new_state, Thread3}}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_ADD_TO_LIST
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , NewValueArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, NewValue, Thread2} = automate_bot_engine_variables:resolve_argument(NewValueArg, Thread, Op),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread2, ListName) of
                      {ok, Value} ->
                          Value;
                      {error, not_found} ->
                          []
                  end,

    %% TODO (optimization) avoid using list++list
    ValueAfter = ValueBefore ++ [NewValue],

    {ok, Thread3 } = automate_bot_engine_variables:set_program_variable(Thread2, ListName, ValueAfter),

    ok = notify_variable_update(ListName, Thread3),
    {ran_this_tick, increment_position(Thread3)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_DELETE_OF_LIST
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , IndexValueArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue, Thread2} = automate_bot_engine_variables:resolve_argument(IndexValueArg, Thread, Op),
    Index = to_int(IndexValue),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread2, ListName) of
                      {ok, Value} ->
                          Value;
                      {error, not_found} ->
                          throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                              , block_id=?UTILS:get_block_id(Op)
                                              })
                  end,

    ValueAfter = automate_bot_engine_naive_lists:remove_nth(ValueBefore, Index),

    {ok, Thread3 } = automate_bot_engine_variables:set_program_variable(Thread2, ListName, ValueAfter),
    ok = notify_variable_update(ListName, Thread3),
    {ran_this_tick, increment_position(Thread3)};

run_instruction(#{ ?TYPE := ?COMMAND_DELETE_ALL_LIST
                 , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                    , ?VALUE := ListName
                                    }
                                 ]
                 }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, Thread2 } = automate_bot_engine_variables:set_program_variable(Thread, ListName, []),
    ok = notify_variable_update(ListName, Thread2),
    {ran_this_tick, increment_position(Thread2)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_INSERT_AT_LIST
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , ValueArg
                                    , IndexArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue, Thread2} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread, Op),
    Index = to_int(IndexValue),
    {ok, Value, Thread3} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread2, Op),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread3, ListName) of
                      {ok, ListOnDB} ->
                          ListOnDB;
                      {error, not_found} ->
                          []
                  end,

    PaddedValue = automate_bot_engine_naive_lists:pad_to_length(
                    ValueBefore, IndexValue - 1, ?LIST_FILL), %% Remember: 1-indexed

    ValueAfter = automate_bot_engine_naive_lists:insert_nth(PaddedValue, Index, Value),

    {ok, Thread4 } = automate_bot_engine_variables:set_program_variable(Thread3, ListName, ValueAfter),
    ok = notify_variable_update(ListName, Thread4),
    {ran_this_tick, increment_position(Thread4)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_REPLACE_VALUE_AT_INDEX
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                       , ?VALUE := ListName
                                       }
                                    , IndexArg
                                    , ValueArg
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, IndexValue, Thread2} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread, Op),
    Index = to_int(IndexValue),
    {ok, Value, Thread3} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread2, Op),
    ValueBefore = case automate_bot_engine_variables:get_program_variable(Thread3, ListName) of
                      {ok, ListOnDB} ->
                          ListOnDB;
                      {error, not_found} ->
                          []
                  end,

    PaddedValue = automate_bot_engine_naive_lists:pad_to_length(
                    ValueBefore, IndexValue - 1, ?LIST_FILL), %% Remember: 1-indexed
    ValueAfter = automate_bot_engine_naive_lists:replace_nth(PaddedValue, Index, Value),

    {ok, Thread4 } = automate_bot_engine_variables:set_program_variable(Thread3, ListName, ValueAfter),
    ok = notify_variable_update(ListName, Thread4),
    {ran_this_tick, increment_position(Thread4)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_CALL_SERVICE
                    , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                     , ?SERVICE_ACTION := Action
                                     , ?SERVICE_CALL_VALUES := Arguments
                                     }
                    }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    {Values, Thread2} = eval_args(Arguments, Thread, Op),

    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId),
    {ok, Thread3, Value} = automate_service_registry_query:call(Module, Action, Values, Thread2, UserId),
    Thread4 = case ?UTILS:get_block_id(Op) of
                  none -> Thread3;
                  BlockId -> automate_bot_engine_variables:set_instruction_memory(Thread3, Value, BlockId)
              end,
    {ran_this_tick, increment_position(Thread4)};

run_instruction(Operation=#{ ?TYPE := <<"services.ui.", UiElement/binary>>
                           , ?ARGUMENTS := Arguments
                           }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->
    {Values, Thread2} = eval_args(Arguments, Thread, Operation),
    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),
    ok = automate_storage:set_widget_value(ProgramId, UiElement, Values),

    %% Trigger element update
    ok = automate_channel_engine:send_to_channel(ChannelId, #{ <<"key">> => ui_events_show
                                                             , <<"subkey">> => UiElement
                                                             , <<"values">> => Values
                                                             } ),
    {ran_this_tick, increment_position(Thread2)};


run_instruction(Operation=#{ ?TYPE := <<"services.", ServiceCall/binary>>
                           , ?ARGUMENTS := Arguments
                           }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    SaveTo = get_save_to(Operation),
    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    ReadArguments = remove_save_to(Arguments, SaveTo),
    {Values, Thread2} = eval_args(ReadArguments, Thread, Operation),

    [ServiceId, Action] = binary:split(ServiceCall, <<".">>),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId),
    {ok, Thread3, Value} = automate_service_registry_query:call(Module, Action, Values, Thread2, UserId),

    {ok, Thread4} = case SaveTo of
                            { index, Index } ->
                                #{ <<"value">> := VariableName
                                 } = lists:nth(Index, Arguments),
                                automate_bot_engine_variables:set_program_variable(
                                  Thread3,
                                  %% Note that erlang is 1-indexed, protocol is 0-indexed
                                  VariableName,
                                  Value);
                            _ ->
                                {ok, Thread3}
                        end,
    {ran_this_tick, increment_position(Thread4)};

run_instruction(Op=#{ ?TYPE := ?MATCH_TEMPLATE_STATEMENT
                    , ?ARGUMENTS := [#{ ?TYPE := ?TEMPLATE_NAME_TYPE
                                      , ?VALUE := TemplateId
                                      }
                                    , Input
                                    ]
                    }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    {ok, InputValue, Thread2} = automate_bot_engine_variables:resolve_argument(Input, Thread, Op),

    case automate_template_engine:match(UserId, Thread2, TemplateId, InputValue) of
        {ok, NewThread, _Value} ->
            {ran_this_tick, increment_position(NewThread)};
        {error, not_found} ->
            {ran_this_tick, finish_thread(Thread2)}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_CUSTOM_SIGNAL
                    , ?ARGUMENTS := [ SignalIdVal
                                    , SignalDataVal
                                    ]
                    }, Thread=#program_thread{ program_id=_ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, ChannelId, Thread2 } = automate_bot_engine_variables:resolve_argument(SignalIdVal, Thread, Op),
    {ok, SignalData, Thread3 } = automate_bot_engine_variables:resolve_argument(SignalDataVal, Thread2, Op),

    ok = automate_channel_engine:send_to_channel(ChannelId, SignalData),

    {ran_this_tick, increment_position(Thread3)};

run_instruction(Op=#{ ?TYPE := ?CONTEXT_SELECT_CONNECTION
                    , ?ARGUMENTS := [ BridgeIdVal
                                    , ConnectionIdVal
                                    ]
                    }, Thread=#program_thread{ position=Position },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, BridgeId, Thread2 } = automate_bot_engine_variables:resolve_argument(BridgeIdVal, Thread, Op),
    {ok, ConnectionId, Thread3 } = automate_bot_engine_variables:resolve_argument(ConnectionIdVal, Thread2, Op),

    case automate_bot_engine_variables:retrieve_instruction_memory(Thread3) of
        {ok, _} ->
            %% Already here, exit the context
            Thread4 = automate_bot_engine_variables:unset_instruction_memory(Thread3),
            {ran_this_tick, increment_position(Thread4)};
        {error, not_found} ->
            Thread4 = automate_bot_engine_variables:set_instruction_memory(
                              Thread3, [ {already_run, true}
                                       , { context_group
                                         , bridge_connection
                                         , {BridgeId, ConnectionId} }]),
            {ran_this_tick, Thread4#program_thread{ position=Position ++ [1] }}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_PRELOAD_GETTER
                    , ?ARGUMENTS := [ Arg=#{ ?TYPE := ?VARIABLE_BLOCK
                                           , ?VALUE := _Block
                                           }
                                    ]
                    }, Thread=#program_thread{ program_id=_ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, _Result, Thread2 } = automate_bot_engine_variables:resolve_argument(Arg, Thread, Op),
    {ran_this_tick, increment_position(Thread2)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_LOG_VALUE
                    , ?ARGUMENTS := [ Arg
                                    ]
                    }, Thread=#program_thread{ program_id=ProgramId },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    {ok, ArgValue, Thread2 } = automate_bot_engine_variables:resolve_argument(Arg, Thread, Op),
    Format = case ArgValue of
                 X when is_binary(X) ->
                     "~s";
                 _ ->
                     "~p"
             end,
    Message = binary:list_to_bin(
                lists:flatten(io_lib:format(Format, [ArgValue]))),
    ok = automate_storage:add_user_generated_log(#user_generated_log_entry{
                                                    severity=debug,
                                                    program_id=ProgramId,
                                                    block_id=?UTILS:get_block_id(Op),
                                                    event_message=Message,
                                                    event_time=os:system_time(millisecond)
                                                   }),
    {ran_this_tick, increment_position(Thread2)};

run_instruction(Op=#{ ?TYPE := ?COMMAND_FORK_EXECUTION
                    , ?ARGUMENTS := Arguments
                    , ?CONTENTS := Flows
                    }, Thread=#program_thread{ program_id=ProgramId, position=Position },
                {?SIGNAL_PROGRAM_TICK, _}) ->

    %% TODO: Consider actively signaling the parent when children end
    case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
        %% Parent thread start, fork children
        {error, not_found} ->
            Thread2 = automate_bot_engine_variables:set_instruction_memory(
                        Thread, #{ already_run => true }),

            {ComputedArgs, Thread3} = eval_args(Arguments, Thread2, Op),
            ContinuationType = case lists:search(fun(X) -> X =:= ?OP_FORK_CONTINUE_ON_FIRST end, ComputedArgs) of
                                   { value, _ } ->
                                       continue_on_first_done;
                                   _ ->
                                       continue_when_all_done
                               end,

            ChildrenIds = lists:map(fun(Index) ->
                                         {ok, NewThreadId } = automate_bot_engine_thread_launcher:launch_thread(
                                                             ProgramId,
                                                             Thread3#program_thread{position=Position ++ [Index, 1]}),
                                            NewThreadId
                                 end, lists:seq(1, length(Flows))),

            Thread4 = automate_bot_engine_variables:set_instruction_memory(
                        Thread3, #{ already_run => true
                                  , children => ChildrenIds
                                  , continuation_type => ContinuationType
                                  }),
            %% Note that position is not incremented, so this instruction keeps
            %% executing until all the children end
            {ran_this_tick, Thread4};
        %% Parent keeps executing and periodically checks if children did finish
        {ok, #{ children := Children, already_run := true, continuation_type := ContinuationType } } ->
            {RemainingChildren, CompletedChildren} = lists:partition(
                                                        fun(ChildId) ->
                                                                {ok, Value} = automate_storage:dirty_is_thread_alive(ChildId),
                                                                Value
                                                        end, Children),
            ForkDone = (((ContinuationType =:= continue_when_all_done) and (length(RemainingChildren) =:= 0))
                        or ((ContinuationType =:= continue_on_first_done) and (length(CompletedChildren) > 0))),
            case ForkDone of
                true ->
                    Thread2 = automate_bot_engine_variables:unset_instruction_memory(Thread),
                    {ran_this_tick, increment_position(Thread2)};
                false ->
                    Thread2 = automate_bot_engine_variables:set_instruction_memory(
                                Thread, #{ already_run => true
                                         , children => RemainingChildren
                                         , continuation_type => ContinuationType
                                         }),
                    {did_not_run, {new_state, Thread2}}
            end;
        %% Children thread, just finish thread
        {ok, #{ already_run := true }} ->
            {stopped, thread_finished}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                       , ?VALUE := VarName
                                       }
                                    ]
                    }, Thread, {?SIGNAL_PROGRAM_TICK, _}) ->
    CurrentValue = case automate_bot_engine_variables:get_program_variable(Thread, VarName) of
                {ok, Val} ->
                    Val;
                {error, not_found} ->
                    not_found
            end,
    case automate_bot_engine_variables:retrieve_instruction_memory(Thread) of
        {error, not_found} ->
            %% Initial run, save current value and wait
            Thread2 = automate_bot_engine_variables:set_instruction_memory(Thread, #{ initial_value => CurrentValue }),
            {did_not_run, {new_state, Thread2}};
        {ok, #{ initial_value := CurrentValue } } ->
            %% Non-initial run, variable value did NOT change (old matches with current)
            {did_not_run, waiting};
        {ok, #{ initial_value := _OtherValue }} ->
            %% Non-initial run, variable DID change
            Thread2 = case ?UTILS:get_block_id(Op) of
                          none ->
                              Thread;
                          Id ->
                              automate_bot_engine_variables:set_instruction_memory(Thread,
                                                                                   CurrentValue,
                                                                                   Id)
                      end,
            {ran_this_tick, increment_position(Thread2)}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                 , ?ARGUMENTS := [ _Block
                                 ]
                 }, _Thread, {?SIGNAL_PROGRAM_TICK, _}) ->
    %% This must not advace on tick, only when a new value (of the listened block) is passed
    {did_not_run, waiting};

run_instruction(Op=#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_BLOCK
                                       , ?VALUE := [ Listened=#{ ?TYPE := <<"services.", MonitorPath/binary>>
                                                               }
                                                   ]
                                       }
                                    ]
                    },
                Thread=#program_thread{ program_id=_ProgramId },
                { ?TRIGGERED_BY_MONITOR, {_MonitorId, Message=#{ <<"key">> := MessageKey, <<"service_id">> := BridgeId }} }) ->

    [ServiceId, MonitorKey] = binary:split(MonitorPath, <<".">>),

    case BridgeId of
        ServiceId ->
            SubKeyMatch = case Listened of
                              #{ ?ARGUMENTS := Arguments } ->
                                  case {?UTILS:get_block_key_subkey(Arguments), Message} of
                                      {{ key_and_subkey, _Key, SubKey }, #{ <<"subkey">> := SubKey }} ->
                                          %% Subkey match
                                          true;
                                      {{key_and_subkey, _Key, _SubKey}, _} ->
                                          %% Subkey NO match
                                          false;
                                      _ ->
                                          true %% Subkey not present
                                  end;
                              _ ->
                                  %% No arguments, so key match is enough
                                  true
                          end,
            KeyMatch = (MonitorKey =:= MessageKey) and SubKeyMatch,
            case KeyMatch of
                true ->
                    %% Save content if appropriate
                    Thread2 = case Message of
                                  #{ ?CHANNEL_MESSAGE_CONTENT := Content } ->
                                      case ?UTILS:get_block_id(Op) of
                                          none ->
                                              Thread;
                                          Id ->
                                              automate_bot_engine_variables:set_instruction_memory(Thread,
                                                                                                   Content,
                                                                                                   Id)
                                      end;
                                  _ -> Thread
                              end,

                    {ran_this_tick, increment_position(Thread2)};
                false ->
                    automate_logging:log_platform(warning,
                                                  io_lib:format("Unexpected signal (key did't match) ~p for block: ~p",
                                                                [Message, Listened])),
                    {did_not_run, waiting}
            end;
        _ ->
            automate_logging:log_platform(warning,
                                          io_lib:format("Unexpected signal (monitor didn't match) ~p for block: ~p",
                                                        [Message, Listened])),
            {did_not_run, waiting}
    end;

run_instruction(Op=#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                    , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_BLOCK
                                       , ?VALUE := [ #{ ?TYPE := ?WAIT_FOR_MONITOR
                                                      , ?ARGUMENTS := MonArgs=#{ ?FROM_SERVICE := ServiceId }
                                                      }
                                                   ]
                                       }
                                    ]
                    },
                Thread,
                { ?TRIGGERED_BY_MONITOR, {_MonitorId, Message=#{ <<"service_id">> := ServiceId  }} }) ->

    Accepted = case MonArgs of
                   #{ <<"key">> := ExpectedKey } ->
                       case Message of
                           #{ <<"key">> := ExpectedKey} ->
                               true;
                           _ ->
                               false
                       end;
                   _ -> %% No key required
                       true
               end,
    case Accepted of
        false ->
            {did_not_run, waiting};
        true ->
            Thread2 = case Message of
                          #{ ?CHANNEL_MESSAGE_CONTENT := Content } ->
                              case ?UTILS:get_block_id(Op) of
                                  none ->
                                      Thread;
                                  Id ->
                                      automate_bot_engine_variables:set_instruction_memory(Thread,
                                                                                           Content,
                                                                                           Id)
                              end;
                          _ -> Thread
                      end,

            {ran_this_tick, increment_position(Thread2)}
    end;

run_instruction(#{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                 , ?ARGUMENTS := [ Block
                                 ]
                 }, _Thread, Message) ->
    automate_logging:log_platform(warning, io_lib:format("Got unexpected signal ~p for block: ~p",
                                                         [Message, Block])),

    {did_not_run, waiting};

run_instruction(#{ ?TYPE := Instruction }, _Thread, Message) ->
    automate_logging:log_platform(
      warning,
      io_lib:format("Unhandled instruction/msg: ~p/~p", [Instruction, Message])),
    {did_not_run, waiting};

run_instruction(#{ <<"contents">> := _Content }, Thread, _Message) ->
    %% Finished code block
    {ran_this_tick, increment_position(Thread)}.

increment_position(Thread = #program_thread{position=Position}) ->
    IncrementedInnermost = increment_innermost(Position),
    FollowInSameLevelState = Thread#program_thread{position=IncrementedInnermost},
    case get_instruction(FollowInSameLevelState) of
        {ok, _} ->
            FollowInSameLevelState;
        {error, element_not_found} ->
            BackToParent = back_to_parent(Position),
            Thread#program_thread{position=BackToParent}
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
-spec run_getter_block(map(), #program_thread{}) -> {ok, any(), #program_thread{}} | {error, not_found}.
run_getter_block(Op, Thread) ->
    case get_block_result(Op, Thread) of
        {error, Reason} ->
            {error, Reason};
        {ok, Result, Thread2} ->
            Thread3 = case ?UTILS:get_block_id(Op) of
                          none ->
                              Thread2;
                          Id ->
                              automate_bot_engine_variables:set_instruction_memory(Thread2,
                                                                                   Result,
                                                                                   Id)
                      end,
            {ok, Result, Thread3}
    end.

%% String operators
-spec get_block_result(map(), #program_thread{}) -> {ok, any(), #program_thread{}} | {error, not_found}.
get_block_result(Op=#{ ?TYPE := ?COMMAND_JOIN
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->

    {[FirstVal, SecondVal], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    {ok, Value} = automate_bot_engine_values:join(FirstVal, SecondVal),
    {ok, Value, Thread2};

get_block_result(Op=#{ ?TYPE := ?COMMAND_STRING_CONTAINS
                     , ?ARGUMENTS := [ Haystack
                                     , Needle
                                     ]
                     }, Thread) ->

    {[HaystackVal, NeedleVal], Thread2} = eval_args([Haystack, Needle], Thread, Op),
    {ok, Value} = automate_bot_engine_values:string_contains(HaystackVal, NeedleVal),
    {ok, Value, Thread2};

get_block_result(Op=#{ ?TYPE := ?COMMAND_JSON
                     , ?ARGUMENTS := [ KeyReference
                                     , MapReference
                                     ]
                     }, Thread) ->

    {[Key, Map], Thread2} = eval_args([KeyReference, MapReference], Thread, Op),
    case automate_bot_engine_values:get_value_by_key(Key, Map) of
        {ok, Value} ->
            {ok, Value, Thread2};
        Error ->
            Error
    end;

%% Templates
get_block_result(Op=#{ ?TYPE := ?MATCH_TEMPLATE_CHECK
                     , ?ARGUMENTS := [#{ ?TYPE := ?TEMPLATE_NAME_TYPE
                                       , ?VALUE := TemplateId
                                       }
                                     , Input
                                     ]
                     }, Thread=#program_thread{ program_id=ProgramId }) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    {ok, InputValue, Thread2} = automate_bot_engine_variables:resolve_argument(Input, Thread, Op),
    case automate_template_engine:match(UserId, Thread2, TemplateId, InputValue) of
        {ok, Thread3, _Value} ->
            {ok, true, Thread3};
        {error, not_found} ->
            {ok, false}
    end;

%% Numeric operators
get_block_result(Op=#{ ?TYPE := ?COMMAND_ADD
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->

    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case automate_bot_engine_values:add(FirstValue, SecondValue) of
        {ok, Value} ->
            {ok, Value, Thread2};
        Error ->
            Error
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_SUBTRACT
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case automate_bot_engine_values:subtract(FirstValue, SecondValue) of
        {ok, Value} ->
            {ok, Value, Thread2};
        Error ->
            Error
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_MULTIPLY
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case automate_bot_engine_values:multiply(FirstValue, SecondValue) of
        {ok, Value} ->
            {ok, Value, Thread2};
        Error ->
            Error
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_DIVIDE
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case automate_bot_engine_values:divide(FirstValue, SecondValue) of
        {ok, Value} ->
            {ok, Value, Thread2};
        Error ->
            Error
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_MODULO
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case automate_bot_engine_values:modulo(FirstValue, SecondValue) of
        {ok, Value} ->
            {ok, Value, Thread2};
        Error ->
            Error
    end;

%% Comparations
get_block_result(Op=#{ ?TYPE := ?COMMAND_LESS_THAN
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    {ok, Value} =  automate_bot_engine_values:is_less_than(FirstValue, SecondValue),
    {ok, Value, Thread2};

get_block_result(Op=#{ ?TYPE := ?COMMAND_GREATER_THAN
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    {ok, Value} = automate_bot_engine_values:is_greater_than(FirstValue, SecondValue),
    {ok, Value, Thread2};

get_block_result(Op=#{ ?TYPE := ?COMMAND_EQUALS
                     , ?ARGUMENTS := Args
                     }, Thread) ->
    {Values, Thread2} = eval_args(Args, Thread, Op),
    {ok, Result} = automate_bot_engine_values:are_equal(Values),
    {ok, Result, Thread2};

%% Boolean operations
get_block_result(Op=#{ ?TYPE := ?COMMAND_AND
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case {FirstValue, SecondValue} of
        {true, true} ->
            {ok, true, Thread2};
        {error, Reason} ->
            {error, Reason};
        {_, _} ->
            {ok, false, Thread2}
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_OR
                     , ?ARGUMENTS := [ First
                                     , Second
                                     ]
                     }, Thread) ->
    {[FirstValue, SecondValue], Thread2} = eval_args([First, Second], Thread, Op),
    %% TODO: Consider how this can be made variadic
    case {FirstValue, SecondValue} of
        {true, _} ->
            {ok, true, Thread2};
        {_, true} ->
            {ok, true, Thread2};
        {error, Reason} ->
            {error, Reason};
        {_, _} ->
            {ok, false, Thread2}
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_NOT
                     , ?ARGUMENTS := [ Value
                                     ]
                     }, Thread) ->
    {ok, Result, Thread2} = automate_bot_engine_variables:resolve_argument(Value, Thread, Op),
    case Result of
        false ->
            {ok, true, Thread2};
        true ->
            {ok, false, Thread2};
        _ ->
            {error, not_found}
    end;

%% Variables
get_block_result(Op=#{ ?TYPE := ?COMMAND_DATA_VARIABLE
                     , ?ARGUMENTS := [ Value
                                     ]
                     }, Thread) ->
    automate_bot_engine_variables:resolve_argument(Value, Thread, Op);

%% List
get_block_result(Op=#{ ?TYPE := ?COMMAND_ITEM_OF_LIST
                     , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                        , ?VALUE := ListName
                                        }
                                     , IndexArg
                                     ]
                     }, Thread) ->
    {ok, IndexValue, Thread2} = automate_bot_engine_variables:resolve_argument(IndexArg, Thread, Op),
    Index = to_int(IndexValue),
    case automate_bot_engine_variables:get_program_variable(Thread2, ListName) of
        {ok, List} ->
            case automate_bot_engine_naive_lists:get_nth(List, Index) of
                {ok, Value } ->
                    {ok, Value, Thread2};
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
    {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread, Op),
    case automate_bot_engine_variables:get_program_variable(Thread2, ListName) of
        {ok, List} ->
            case automate_bot_engine_naive_lists:get_item_num(List, Value) of
                {ok, Index} ->
                    {ok, Index, Thread2};
                {error, not_found} ->
                    {error, not_found}
                end;
        {error, not_found} ->
            throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(#{ ?TYPE := ?COMMAND_LENGTH_OF_LIST
                  , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                     , ?VALUE := ListName
                                     }
                                  ]
                  }, Thread) ->
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            {ok, Value} = automate_bot_engine_naive_lists:get_length(List),
            {ok, Value, Thread};
        {error, not_found} ->
            {ok, 0, Thread}
    end;

get_block_result(Op=#{ ?TYPE := ?COMMAND_LIST_CONTAINS_ITEM
                     , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                        , ?VALUE := ListName
                                        }
                                     , ValueArg
                                     ]
                     }, Thread) ->
    {ok, Value, Thread2} = automate_bot_engine_variables:resolve_argument(ValueArg, Thread, Op),
    case automate_bot_engine_variables:get_program_variable(Thread2, ListName) of
        {ok, List} ->
            Found = automate_bot_engine_naive_lists:contains(List, Value),
            {ok, Found, Thread2};
        {error, not_found} ->
            {ok, false, Thread2}
    end;

get_block_result(#{ ?TYPE := <<"monitor.retrieve.", MonitorId/binary>>
                  , ?ARGUMENTS := []
                  }, Thread) ->
    case automate_monitor_engine:get_last_monitor_result(MonitorId) of
        {ok, Result} ->
            {ok, Result, Thread}
    end;

get_block_result(Op=#{ ?TYPE := <<"services.", ServiceCall/binary>>
                     , ?ARGUMENTS := Arguments
                     }, Thread=#program_thread{ program_id=ProgramId }) ->

    {ok, UserId} = automate_storage:get_program_owner(ProgramId),

    {Values, Thread2} = eval_args(Arguments, Thread, Op),

    [ServiceId, Action] = binary:split(ServiceCall, <<".">>),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId),
    {ok, Thread3, Value} = automate_service_registry_query:call(Module, Action, Values, Thread2, UserId),
    {ok, Value, Thread3};

get_block_result(Op=#{ ?TYPE := <<"services.", _ServiceCall/binary>> }, Thread) ->
    get_block_result(Op#{ ?ARGUMENTS => [] }, Thread);

get_block_result(Op=#{ ?TYPE := ?COMMAND_CALL_SERVICE
                     , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                      , ?SERVICE_ACTION := Action
                                      , ?SERVICE_CALL_VALUES := Args
                                      }
                     }, Thread=#program_thread{ program_id=PID }) ->

    {ok, #user_program_entry{ owner=Owner }} = automate_storage:get_program_from_id(PID),
    Arguments = case Args of
                    %% This first form was generated on the Scratch's
                    %% serialization, but it's not found on the getters and it's
                    %% redundant.
                    #{ ?ARGUMENTS := A } -> A;
                    _ -> Args
                end,

    {Values, Thread2} = eval_args(Arguments, Thread, Op),

    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId),
    {ok, Thread3, Value} = automate_service_registry_query:call(Module, Action, Values, Thread2, Owner),
    {ok, Value, Thread3};

get_block_result(Op=#{ ?TYPE := ?COMMAND_LIST_GET_CONTENTS
                     , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_LIST
                                        , ?VALUE := ListName
                                        }
                                     ]
                     }, Thread) ->
    case automate_bot_engine_variables:get_program_variable(Thread, ListName) of
        {ok, List} ->
            {ok, List, Thread};
        {error, not_found} ->
            throw(#program_error{ error=#list_not_set{ list_name=ListName }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(Op=#{ ?TYPE := ?FLOW_LAST_VALUE
                     , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_CONSTANT
                                        , ?VALUE := BlockId
                                        }
                                     , #{ ?TYPE := ?VARIABLE_CONSTANT
                                        , ?VALUE := _Index
                                        }
                                     ]
                     }, Thread) ->
    case automate_bot_engine_variables:retrieve_instruction_memory(Thread, BlockId) of
        {ok, Value} ->
            {ok, Value, Thread};
        {error, not_found} ->
            throw(#program_error{ error=#memory_not_set{ block_id=BlockId }
                                , block_id=?UTILS:get_block_id(Op)
                                })
    end;

get_block_result(#{ ?TYPE := ?COMMAND_GET_THREAD_ID
                  }, Thread=#program_thread{ thread_id=ThreadId }) ->
    {ok, ThreadId, Thread};

%% Fail
get_block_result(Block, _Thread) ->
    automate_logging:log_platform(error, io_lib:format("Don't know how to get result from: ~p~n",
                                                       [Block])),
    throw(#program_error{ error=#unknown_operation{}
                        , block_id=?UTILS:get_block_id(Block)
                        }).


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

-spec eval_args([any()], #program_thread{}, map()) -> {[any()], #program_thread{}}.
eval_args(Arguments, Thread, Op) ->
    { Thread2, RevValues } = lists:foldl(
                               fun(Arg, {UpdThread, Values}) ->
                                       {ok, Value, UpdThread2} = automate_bot_engine_variables:resolve_argument(Arg, UpdThread, Op),
                                       {UpdThread2, [ Value | Values ]}
                               end,
                               { Thread, [ ] }, Arguments),
    %% This lists:reverse could be avoided if we used `lists:foldr` instead of `lists:foldl`.
    %% But according to erlang documentation [ http://erlang.org/doc/man/lists.html#foldr-3 ]
    %% > foldl/3 is tail recursive and is usually preferred to foldr/3.
    %%
    %% Still, argument lists are going to be so small that the difference does not matter.
    %%
    %% With this in mind, it might be preferrable to read the arguments from
    %% left to right, just to make thinking more intuitive whenever a bug
    %% regarding argument parsing arises. (This is considering the reader's
    %% native language is left-to-right, which might not be true...)
    {lists:reverse(RevValues), Thread2}.

-spec notify_variable_update(VariableName :: binary(), #program_thread{}) -> ok | {error, _}.
notify_variable_update(VariableName, #program_thread{ program_id=ProgramId }) ->
    case automate_storage:get_program_from_id(ProgramId) of
        {ok, #user_program_entry{ program_channel=ChannelId }} ->
            automate_channel_engine:send_to_channel(ChannelId, #{ <<"key">> => variable_events
                                                                  %% This canonicalization is done also on the channel engine, but it's not saved to the subkey
                                                                , <<"subkey">> => automate_channel_engine_utils:canonicalize_selector(VariableName)
                                                                });
        {error, Reason} ->
            {error, Reason}
    end.
