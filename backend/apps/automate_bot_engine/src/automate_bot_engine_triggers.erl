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
    [get_expected_action_from_trigger(Trigger, Permissions, ProgramId) || Trigger <- Triggers ].

-spec get_expected_action_from_trigger(#program_trigger{}, #program_permissions{}, binary()) -> atom().
%% TODO: return a more specific monitor
get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := ?WAIT_FOR_MONITOR
                                                             , ?ARGUMENTS := #{ ?MONITOR_ID := MonitorId }
                                                             }}, _Permissions, _ProgramId) ->
    automate_channel_engine:listen_channel(MonitorId),
    ?TRIGGERED_BY_MONITOR;

get_expected_action_from_trigger(#program_trigger{condition=#{ ?TYPE := <<"services.", MonitorPath/binary>>
                                                             , ?ARGUMENTS := Arguments
                                                             }},
                                 #program_permissions{owner_user_id=UserId}, _ProgramId) ->
    [ServiceId, _MonitorKey] = binary:split(MonitorPath, <<".">>),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, MonitorId } = automate_service_registry_query:get_monitor_id(Module, UserId),

    case get_block_key_subkey(Arguments) of
        { key_and_subkey, Key, SubKey } ->
            automate_channel_engine:listen_channel(MonitorId, { Key, SubKey });
        { key, Key } ->
            automate_channel_engine:listen_channel(MonitorId, { Key });
        { not_found } ->
            automate_channel_engine:listen_channel(MonitorId)
    end,
    ?TRIGGERED_BY_MONITOR;

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
    io:fwrite("[WARN][Bot/Triggers][ProgId=~p] Unknown trigger: ~p~n",  [ProgramId, Trigger]),
    ?SIGNAL_PROGRAM_TICK.

get_block_key_subkey(#{ <<"key">> := Key
                      , <<"subkey">> := #{ <<"type">> := <<"constant">>
                                         , <<"value">> := SubKey
                                         }
                      }) ->
    { key_and_subkey, Key, SubKey };
get_block_key_subkey(#{ <<"key">> := Key }) ->
    {key, Key};
get_block_key_subkey(_) ->
    { not_found }.

get_subkey_value(#{ <<"subkey">> := SubKey }) ->
    {ok, SubKey};
get_subkey_value(_) ->
    {error, not_found}.


%%%% Thread creation
%%% Monitors
%% If any value is OK
-spec trigger_thread(#program_trigger{}, {atom(), any()}, #program_state{}) -> 'false' | {'true', #program_thread{}}.
trigger_thread(#program_trigger{ condition=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                            , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := MonitorId
                                                                         , ?MONITOR_EXPECTED_VALUE := ?MONITOR_ANY_VALUE
                                                                         }
                                            }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }} },
               #program_state{program_id=ProgramId}) ->


    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },


    {ok, ThreadWithSavedValue} = case MonitorArgs of
                                     #{ ?MONITOR_SAVE_VALUE_TO := SaveTo } ->
                                         save_value(Thread, SaveTo, MessageContent);
                                     _ ->
                                         {ok, Thread}
                                 end,

    {ok, NewThread} = automate_bot_engine_variables:set_last_monitor_value(
                        ThreadWithSavedValue, MonitorId, FullMessage),

    {true, NewThread};

%% With matching value
trigger_thread(#program_trigger{ condition= Op=#{ ?TYPE := ?WAIT_FOR_MONITOR_COMMAND
                                               , ?ARGUMENTS := MonitorArgs=#{ ?MONITOR_ID := MonitorId
                                                                            , ?MONITOR_EXPECTED_VALUE := Argument
                                                                            }
                                               }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, {MonitorId, FullMessage=#{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }} },
               #program_state{program_id=ProgramId}) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    {ok, ThreadWithSavedValue} = case MonitorArgs of
                                     #{ ?MONITOR_SAVE_VALUE_TO := SaveTo } ->
                                         save_value(Thread, SaveTo, MessageContent);
                                     _ ->
                                         {ok, Thread}
                                 end,

    case automate_bot_engine_variables:resolve_argument(Argument, ThreadWithSavedValue, Op) of
        {ok, MessageContent} ->
            {ok, NewThread} = automate_bot_engine_variables:set_last_monitor_value(
                                ThreadWithSavedValue, MonitorId, FullMessage),
            {true, NewThread};
        {ok, _Found} ->
            %% io:format("No match. Expected “~p”, found “~p”~n", [MessageContent, Found]),
            false
    end;

%% Bridge channel
trigger_thread(#program_trigger{ condition= Op=#{ ?TYPE := <<"services.", MonitorPath/binary>>
                                               , ?ARGUMENTS := MonitorArgs
                                               }
                               , subprogram=Program
                               },
               { ?TRIGGERED_BY_MONITOR, { TriggeredMonitorId
                                        , FullMessage=#{ <<"key">> := TriggeredKey }
                                        } },
               #program_state{ program_id=ProgramId
                                          , permissions=#program_permissions{owner_user_id=UserId}}) ->

    Thread = #program_thread{ position=[1]
                            , program=Program
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            },

    [ServiceId, FunctionName] = binary:split(MonitorPath, <<".">>),

    KeyMatch = case get_block_key_subkey(MonitorArgs) of
                   { key_and_subkey, Key, SubKey } ->
                       case get_subkey_value(FullMessage) of
                           {ok, SubKey} ->
                               Key == TriggeredKey;
                           E ->
                               false
                       end;
                   { key, Key } ->
                       Key == TriggeredKey;
                   { not_found } ->
                       FunctionName == TriggeredKey
               end,
    case KeyMatch of
        false ->
            false;
        true ->
            {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
            {ok, MonitorId } = automate_service_registry_query:get_monitor_id(Module, UserId),
            MatchingContent = case MonitorArgs of
                                  #{ ?MONITOR_EXPECTED_VALUE := ExpectedValue } ->
                                      {ok, ResolvedExpectedValue} = automate_bot_engine_variables:resolve_argument(
                                                                      ExpectedValue, Thread, Op),
                                      ActualValue = maps:get(?CHANNEL_MESSAGE_CONTENT, FullMessage, none),
                                      ResolvedExpectedValue == ActualValue;
                                  _ ->
                                      true
                              end,
            case {MonitorId, MatchingContent} of
                {TriggeredMonitorId, true} ->
                    {ok, ThreadWithSavedValue} = case {MonitorArgs, FullMessage} of
                                                     { #{ ?MONITOR_SAVE_VALUE_TO := SaveTo }
                                                     , #{ ?CHANNEL_MESSAGE_CONTENT := MessageContent }
                                                     } ->
                                                         save_value(Thread, SaveTo, MessageContent);
                                                     _ ->
                                                         {ok, Thread}
                                                 end,

                    {ok, NewThread} = automate_bot_engine_variables:set_last_monitor_value(
                                        ThreadWithSavedValue, MonitorId, FullMessage),
                    {true, NewThread};
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
