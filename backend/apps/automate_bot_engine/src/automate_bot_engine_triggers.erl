-module(automate_bot_engine_triggers).

%% API
-export([ get_expected_signals/1
        , get_triggered_threads/2
        ]).

-define(SERVER, ?MODULE).
-define(UTILS, automate_bot_engine_utils).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec get_expected_signals(#program_state{}) -> {ok, [atom()]}.
get_expected_signals(#program_state{program_id=ProgramId, triggers=Triggers, permissions=Permissions}) ->
    {ok, get_expected_signals_from_triggers(Triggers, Permissions, ProgramId)}.


-spec get_triggered_threads(#program_state{}, {atom(), any()}) -> {ok, [#program_thread{}]}.
get_triggered_threads(Program=#program_state{triggers=Triggers}, Signal) ->
    { ok
    , lists:filtermap(fun(Thread) -> trigger_thread(Thread, Signal, Program) end,
                      Triggers)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%% Expected signals
-spec get_expected_signals_from_triggers([#program_trigger{}], #program_permissions{}, binary()) -> [atom()].
get_expected_signals_from_triggers(Triggers, Permissions, ProgramId) ->
    lists:filtermap(fun(Trigger) ->
                            try get_expected_action_from_trigger(Trigger, Permissions, ProgramId) of
                                false ->
                                    false;
                                Result ->
                                    {true, Result}
                            catch ErrorNS:Error:StackTrace ->
                                    automate_logging:log_platform(error, ErrorNS, Error, StackTrace),
                                    false
                            end
                    end, Triggers).

-spec get_expected_action_from_trigger(#program_trigger{}, #program_permissions{}, binary()) -> atom().
%% TODO: return a more specific monitor
get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?WAIT_FOR_MONITOR
                                                             , ?ARGUMENTS := #{ ?MONITOR_ID := #{ ?FROM_SERVICE := ServiceId } }
                                                             }}, #program_permissions{owner_user_id=UserId}, _ProgramId) ->
    ok = automate_service_registry_query:listen_service(ServiceId, UserId, { undefined, undefined }),
    ?TRIGGERED_BY_MONITOR;
get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?WAIT_FOR_MONITOR
                                                             , ?ARGUMENTS := #{ ?MONITOR_ID := MonitorId }
                                                             }}, _Permissions, ProgramId) when is_binary(MonitorId) ->
    ok = automate_channel_engine:listen_channel(MonitorId),
    ?TRIGGERED_BY_MONITOR;


get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := <<"services.ui.", UiMonitorPath/binary>>
                                                             }},
                                 #program_permissions{owner_user_id=UserId}, ProgramId) ->

    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),
    automate_channel_engine:listen_channel(ChannelId, { ui_events, UiMonitorPath }),
    ?TRIGGERED_BY_MONITOR;

get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?COMMAND_DATA_VARIABLE_ON_CHANGE
                                                             , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                                                                , ?VALUE := Variable
                                                                                }
                                                                             ]
                                                             }},
                                 #program_permissions{owner_user_id=UserId}, ProgramId) ->

    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),
    automate_channel_engine:listen_channel(ChannelId, { variable_events, Variable }),
    ?TRIGGERED_BY_MONITOR;

get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?FLOW_ON_BLOCK_RUN
                                                             , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_CONSTANT
                                                                                , ?VALUE := BlockId
                                                                                }
                                                                             ,  _ChangeIndex
                                                                             ]
                                                             }},
                                 #program_permissions{owner_user_id=UserId}, ProgramId) ->

    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),
    automate_channel_engine:listen_channel(ChannelId, { block_run_events, BlockId }),
    ?TRIGGERED_BY_MONITOR;

get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := <<"services.", MonitorPath/binary>>
                                                             , ?ARGUMENTS := Arguments
                                                             }},
                                 #program_permissions{owner_user_id=UserId}, ProgramId) ->
    [ServiceId, _MonitorKey] = binary:split(MonitorPath, <<".">>),
    case automate_service_registry:get_service_by_id(ServiceId) of
        {ok, #{ module := Module }} ->
            case ?UTILS:get_block_key_subkey(Arguments) of
                { key_and_subkey, Key, SubKey } ->
                    ok = automate_service_registry_query:listen_service(ServiceId, UserId, { Key, SubKey });
                { key, Key } ->
                    ok = automate_service_registry_query:listen_service(ServiceId, UserId, { Key, undefined });
                { not_found } ->
                    ok = automate_service_registry_query:listen_service(ServiceId, UserId, { undefined, undefined })
            end,

            ?TRIGGERED_BY_MONITOR;
        {error, Reason} ->
            automate_logging:log_program_error(
              #user_program_log_entry{ program_id=ProgramId
                                     , thread_id=none
                                     , owner=UserId
                                     , block_id=undefined
                                     , event_data={error, Reason}
                                     , event_message=binary:list_to_bin(
                                                       lists:flatten(io_lib:format("Error finding service for signal. Might not be active anymore.", [])))
                                     , event_time=erlang:system_time(millisecond)
                                     , severity=warning
                                     , exception_data=none
                                     }),
            false
    end;

get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?SIGNAL_PROGRAM_CUSTOM
                                                             , ?ARGUMENTS := [ #{ ?TYPE := <<"constant">>
                                                                                , ?VALUE := ChannelId
                                                                                }
                                                                             , _SaveToVal
                                                                             ]
                                                             }}, #program_permissions{}, _ProgramId) ->

    automate_channel_engine:listen_channel(ChannelId),
    ?TRIGGERED_BY_MONITOR;

%% By default let's suppose no special data is needed to keep the program running
get_expected_action_from_trigger(Trigger, _Permissions, ProgramId) ->
    %% io:fwrite("[WARN][Bot/Triggers][ProgId=~p] Unknown trigger: ~p~n",  [ProgramId, Trigger]),
    ?SIGNAL_PROGRAM_TICK.

%%%% Thread creation
%%% Monitors
%% If any value is OK
-spec trigger_thread(#program_trigger{}, {atom(), any()}, #program_state{}) -> 'false' | {'true', #program_thread{}}.
trigger_thread(Trigger=#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                                    , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := #{ ?FROM_SERVICE := ServiceId }
                                                                                 , ?MONITOR_EXPECTED_VALUE := ?MONITOR_ANY_VALUE
                                                                                 , ?MONITOR_KEY := MonitorKey
                                                                                 }
                                                    }
                                       , subprogram=Program
                                       },
               { ?TRIGGERED_BY_MONITOR, {_MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent
                                                                  , <<"service_id">> := ServiceId
                                                                  , ?MONITOR_KEY := MsgKey
                                                                  }} },
               #program_state{program_id=ProgramId}) ->
    case MonitorKey == MsgKey of
        true ->
            trigger_thread_with_matching_message(Program, ProgramId, {service, ServiceId}, MonitorArgs, MessageContent, FullMessage, Trigger);
        false ->
            false
    end;

trigger_thread(Trigger=#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                                    , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := #{ ?FROM_SERVICE := ServiceId }
                                                                                 , ?MONITOR_EXPECTED_VALUE := ?MONITOR_ANY_VALUE
                                                                                 }
                                                    }
                                       , subprogram=Program
                                       },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent, <<"service_id">> := ServiceId }} },
               #program_state{program_id=ProgramId}) ->
    trigger_thread_with_matching_message(Program, ProgramId, {service, ServiceId}, MonitorArgs, MessageContent, FullMessage, Trigger);


trigger_thread(Trigger=#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                                    , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := MonitorId
                                                                                 , ?MONITOR_EXPECTED_VALUE := ?MONITOR_ANY_VALUE
                                                                                 }
                                                    }
                                       , subprogram=Program
                                       },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }} },
               #program_state{program_id=ProgramId}) ->
    trigger_thread_with_matching_message(Program, ProgramId, {channel, MonitorId}, MonitorArgs, MessageContent, FullMessage, Trigger);

%% With matching value
trigger_thread(Trigger=#program_trigger{ condition= Op=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                                        , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := #{ ?FROM_SERVICE := ServiceId }
                                                                                     , ?MONITOR_EXPECTED_VALUE := Argument
                                                                                     }
                                                        }
                                       , subprogram=Program
                                       },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent, <<"service_id">> := ServiceId }} },
               #program_state{program_id=ProgramId}) ->
    {true, Thread} = trigger_thread_with_matching_message(Program, ProgramId, {service, ServiceId}, MonitorArgs, MessageContent, FullMessage, Trigger),
    case automate_bot_engine_variables:resolve_argument(Argument, Thread, Op) of
        {ok, MessageContent, UpdatedThread} ->
            {true, Thread};
        {ok, Found, _DiscardedThread} ->
            %% io:format("No match. Expected “~p”, found “~p”~n", [MessageContent, Found]),
            false
    end;

trigger_thread(Trigger=#program_trigger{ condition= Op=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                                        , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := MonitorId
                                                                                     , ?MONITOR_EXPECTED_VALUE := Argument
                                                                                     }
                                                        }
                                       , subprogram=Program
                                       },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }} },
               #program_state{program_id=ProgramId}) when is_binary(MonitorId) ->
    {true, Thread} = trigger_thread_with_matching_message(Program, ProgramId, {channel, MonitorId}, MonitorArgs, MessageContent, FullMessage, Trigger),
    case automate_bot_engine_variables:resolve_argument(Argument, Thread, Op) of
        {ok, MessageContent, UpdatedThread} ->
            {true, Thread};
        {ok, Found, _DiscardedThread} ->
            %% io:format("No match. Expected “~p”, found “~p”~n", [MessageContent, Found]),
            false
    end;

%% UI channel
trigger_thread(#program_trigger{ condition=#{ ?TYPE := <<"services.ui.", UiMonitorPath/binary>>
                                            }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, { _MonitorId
                                        , #{ <<"key">> := ui_events, <<"subkey">> := UiMonitorPath, <<"value">> := Value }
                                        } },
               #program_state{ program_id=ProgramId
                             , permissions=#program_permissions{owner_user_id=_UserId}}) ->

    #{ <<"connection">> := Source, <<"ui_data">> := UiData } = Value,

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{ ?UI_TRIGGER_VALUES => #{ ?UI_TRIGGER_CONNECTION => Source, ?UI_TRIGGER_DATA => UiData } }
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },
    {true, Thread};


trigger_thread(#program_trigger{condition=#{ ?TYPE := ?COMMAND_DATA_VARIABLE_ON_CHANGE
                                           , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_VARIABLE
                                                              , ?VALUE := OperationVariable
                                                              }
                                                           ]
                                           }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, { _MonitorId
                                        , FullMessage=#{ <<"key">> := variable_events, <<"subkey">> := ReceivedVariable }
                                        } },
               #program_state{ program_id=ProgramId
                             , permissions=#program_permissions{owner_user_id=_UserId}}) ->

    %% Manage subkey canonicalization
    case automate_channel_engine_utils:canonicalize_selector(OperationVariable) of
        ReceivedVariable ->
            %% Match!
            Thread = #program_thread{ position=[1]
                                    , program=Program
                                    , global_memory=#{}
                                    , instruction_memory=#{}
                                    , program_id=ProgramId
                                    , thread_id=undefined
                                    },
            {true, Thread};
        _ ->
            false
    end;


trigger_thread(#program_trigger{condition=#{ ?TYPE := ?FLOW_ON_BLOCK_RUN
                                           , ?ARGUMENTS := [ #{ ?TYPE := ?VARIABLE_CONSTANT
                                                              , ?VALUE := BlockId
                                                              }
                                                           ,  _ChangeIndex
                                                           ]
                                           }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, { _MonitorId
                                        , FullMessage=#{ <<"key">> := block_run_events, <<"subkey">> := BlockId}
                                        } },
               #program_state{ program_id=ProgramId
                             , permissions=#program_permissions{owner_user_id=_UserId}}) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    Thread2 = case FullMessage of
                  #{ <<"value">> := Value } ->
                      automate_bot_engine_variables:set_instruction_memory(Thread, Value, BlockId);
                  _ -> Thread
              end,
    {true, Thread2};

%% Bridge channel
trigger_thread(#program_trigger{ condition= Op=#{ ?TYPE := <<"services.", MonitorPath/binary>>
                                                , ?ARGUMENTS := MonitorArgs
                                                }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, { MonitorId
                                        , FullMessage=#{ <<"key">> := TriggeredKey, <<"service_id">> := BridgeId }
                                        } },
               #program_state{ program_id=ProgramId
                             , permissions=#program_permissions{owner_user_id=_UserId}}) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    [ServiceId, FunctionName] = binary:split(MonitorPath, <<".">>),

    KeyMatch = case ?UTILS:get_block_key_subkey(MonitorArgs) of
                   { key_and_subkey, Key, SubKey } ->
                       case ?UTILS:get_subkey_value(FullMessage) of
                           {ok, TriggeredSubKey} ->
                               (Key == TriggeredKey) and (string:lowercase(SubKey) == string:lowercase(TriggeredSubKey));
                           _ ->
                               false
                       end;
                   { key, Key } ->
                       Key == TriggeredKey;
                   { not_found } ->
                       FunctionName == TriggeredKey
               end,

    case KeyMatch and (BridgeId == ServiceId) of
        false ->
            false;
        true ->
            {MatchingContent, Thread2} = case MonitorArgs of
                                             #{ ?MONITOR_EXPECTED_VALUE := ExpectedValue } ->
                                                 {ok, ResolvedExpectedValue, UpdatedThread} = automate_bot_engine_variables:resolve_argument(
                                                                                                ExpectedValue, Thread, Op),
                                                 ActualValue = maps:get(?CHANNEL_MESSAGE_CONTENT, FullMessage, none),
                                                 {ResolvedExpectedValue == ActualValue, UpdatedThread};
                                             _ ->
                                                 {true, Thread}
                                         end,
            case MatchingContent of
                true ->
                    {ok, ThreadWithSavedValue} = case {MonitorArgs, FullMessage} of
                                                     { #{ ?MONITOR_SAVE_VALUE_TO := SaveTo }
                                                     , #{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }
                                                     } ->
                                                         save_value(Thread2, SaveTo, MessageContent);
                                                     _ ->
                                                         {ok, Thread2}
                                                 end,

                    {ok, NewThread} = automate_bot_engine_variables:set_last_bridge_value(
                                        ThreadWithSavedValue, ServiceId, FullMessage),

                    SavedThread = case {?UTILS:get_block_id(Op), FullMessage} of
                                      {undefined, _} ->
                                          NewThread;
                                      {BlockId, #{ ?CHANNEL_MESSAGE_CONTENT := Content }} ->
                                          automate_bot_engine_variables:set_instruction_memory(NewThread,
                                                                                               Content,
                                                                                               BlockId);
                                      _ ->
                                          NewThread
                                  end,

                    {true, SavedThread};
                _ ->
                    false
            end
    end;

%% Custom trigger
trigger_thread(#program_trigger{ condition=#{ ?TYPE := ?SIGNAL_PROGRAM_CUSTOM
                                            , ?ARGUMENTS := [ #{ ?TYPE := <<"constant">>
                                                               , ?VALUE := ChannelId
                                                               }
                                                            ,  SaveTo
                                                            ]
                                            }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, { ChannelId
                                        , MessageContent
                                        } },
               #program_state{ program_id=ProgramId }) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    case SaveTo of
        #{ ?TYPE := ?VARIABLE_VARIABLE } ->
            save_value(Thread, SaveTo, MessageContent);
        _ ->
            ok
    end,
    {true, Thread};

%% If no match is found, don't create a thread
trigger_thread(Trigger, Message, ProgramState) ->
    notify_trigger_not_matched(Trigger, Message, ProgramState),
    false.

%%%===================================================================
%%% Aux functions
%%%===================================================================
trigger_thread_with_matching_message(Program, ProgramId, Channel, MonitorArgs, MessageContent, FullMessage,
                                     #program_trigger{condition=Condition}) ->
    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    {ok, ThreadWithSavedValue} = case MonitorArgs of
                                     #{ ?MONITOR_SAVE_VALUE_TO := SaveTo } ->
                                         save_value(Thread, SaveTo, MessageContent);
                                     _ ->
                                         {ok, Thread}
                                 end,
    Thread2 = case Channel of
                  {channel, ChannelId} ->
                      case automate_service_port_engine:get_channel_origin_bridge(ChannelId) of
                          {ok, ServiceId} ->
                              {ok, Thread1} = automate_bot_engine_variables:set_last_bridge_value(
                                                ThreadWithSavedValue, ServiceId, FullMessage),
                              Thread1;
                          {error, not_found} ->
                              Thread
                      end;
                  {service, ServiceId} ->
                      {ok, Thread1} = automate_bot_engine_variables:set_last_bridge_value(
                                        ThreadWithSavedValue, ServiceId, FullMessage),
                      Thread1
              end,
    NewThread = case Condition of
                    #{ ?BLOCK_ID := BlockId } ->
                        automate_bot_engine_variables:set_instruction_memory(
                          Thread2, FullMessage, BlockId);
                    _ ->
                        Thread2
                end,

    {true, NewThread}.

save_value(Thread, #{ ?TYPE := ?VARIABLE_VARIABLE
                    , ?VALUE := VariableName
                    }, Value) ->
    automate_bot_engine_variables:set_program_variable(Thread, VariableName, Value).


-ifdef(TEST).
notify_trigger_not_matched(_Trigger, { triggered_by_monitor
                                     , {<<"9b438e6e-1922-4253-a0bb-6af4435ba65f">>, _}},
                           _Program) ->
    ok;
notify_trigger_not_matched(_Trigger, {tick, _}, _Program) ->
    ok;
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
%%     io:format("Trigger (~p) not matching (~p) ~n", [Message, Trigger]);
notify_trigger_not_matched(_Trigger, _Message, _Program) ->
    ok.
-endif.
