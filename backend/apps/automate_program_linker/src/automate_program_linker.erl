-module(automate_program_linker).

-export([ link_program/2
        ]).

-include("../../automate_service_registry/src/records.hrl").
-include("../../automate_bot_engine/src/instructions.hrl").
-include("records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec link_program(program(), binary()) -> {ok, program()}.
link_program(Program = #{ <<"blocks">> := Blocks },
             UserId) ->
    io:fwrite("Linking: ~p~n", [Program]),
    RelinkedBlocks = lists:map(fun (Subprogram) -> relink_subprogram(Subprogram, UserId) end, Blocks),
    {ok, Program#{ <<"blocks">> => RelinkedBlocks }}.


relink_subprogram(Subprogram, UserId) ->
    lists:map(fun (Block) -> relink_block(Block, UserId) end, Subprogram).

%% Relink chat_say
relink_block(Block=#{ ?TYPE := <<"chat_say_on_channel">>, ?ARGUMENTS := Arguments }, UserId) ->
    Block#{ ?TYPE := ?COMMAND_CALL_SERVICE
          , ?ARGUMENTS := #{ ?SERVICE_ACTION => send_chat_on_channel
                           , ?SERVICE_ID => automate_services_telegram:get_uuid()
                           , ?SERVICE_CALL_VALUES => lists:map(
                                                       fun(Arg) ->
                                                               relink_block_values(Arg, UserId)
                                                       end,
                                                       Arguments)
                           }
          };

relink_block(Block=#{ ?TYPE := <<"chat_say">>, ?ARGUMENTS := Arguments }, UserId) ->
    Block#{ ?TYPE := ?COMMAND_CALL_SERVICE
          , ?ARGUMENTS := #{ ?SERVICE_ACTION => send_chat
                           , ?SERVICE_ID => automate_services_telegram:get_uuid()
                           , ?SERVICE_CALL_VALUES => lists:map(
                                                       fun(Arg) ->
                                                               relink_block_values(Arg, UserId)
                                                       end,
                                                       Arguments)
                           }
          };


%% Relink service monitor
relink_block(Block, UserId) ->
    B1 = relink_block_contents(Block, UserId),
    B2 = relink_block_args(B1, UserId),
    relink_block_values(B2, UserId).

relink_block_contents(Block=#{ ?CONTENTS := Contents
                             }, UserId) when is_list(Contents) ->
    Block#{ ?CONTENTS => lists:map(fun(B) -> relink_block(B, UserId) end,
                                   Contents)
          };

relink_block_contents(Block, UserId) ->
    Block.

relink_block_args(Block=#{ ?ARGUMENTS := Arguments
                         }, UserId) when is_map(Arguments) ->
    B1 = relink_monitor_id(Block, UserId),
    B1;

relink_block_args(Block, _UserId) ->
    Block.


relink_monitor_id(Block=#{ ?ARGUMENTS := Args=
                               #{ ?MONITOR_ID := #{ ?FROM_SERVICE := ServiceId } } }
                 , UserId
                 ) ->
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, MonitorId } = Module:get_monitor_id(UserId),
    Block#{ ?ARGUMENTS := Args#{ ?MONITOR_ID :=  MonitorId } }.

%%%% Relink values
relink_block_values(Block=#{ ?VALUE := Value
                           }, _UserId) when is_list(Value) ->
    io:fwrite("Relinking: ~p~n", [Block]),
    Block#{ ?VALUE => lists:map(fun relink_value/1, Value) };

relink_block_values(Block, _UserId) ->
    io:fwrite("Cannot relink: ~p~n", [Block]),
    Block.


%% Relink time
relink_value(Value = #{ ?TYPE := <<"time_get_utc_hour">>
                      }) ->
    #{ ?TYPE => ?COMMAND_CALL_SERVICE
     , ?ARGUMENTS => #{ ?SERVICE_ACTION => get_utc_hour
                      , ?SERVICE_ID => automate_services_time:get_uuid()
                      , ?SERVICE_CALL_VALUES => Value
                      }
     };

relink_value(Value = #{ ?TYPE := <<"time_get_utc_minute">>
                      }) ->
    #{ ?TYPE => ?COMMAND_CALL_SERVICE
     , ?ARGUMENTS => #{ ?SERVICE_ACTION => get_utc_minute
                      , ?SERVICE_ID => automate_services_time:get_uuid()
                      , ?SERVICE_CALL_VALUES => Value
                      }
     };

relink_value(Value = #{ ?TYPE := <<"time_get_utc_seconds">>
                      }) ->
    #{ ?TYPE => ?COMMAND_CALL_SERVICE
     , ?ARGUMENTS => #{ ?SERVICE_ACTION => get_utc_seconds
                      , ?SERVICE_ID => automate_services_time:get_uuid()
                      , ?SERVICE_CALL_VALUES => Value
                      }
     };

%%%% ^^^ Service linking
relink_value(Block=#{ ?ARGUMENTS := Arguments }) ->
    io:fwrite("---> ~p~n", [Block]),
    Block#{ ?ARGUMENTS => lists:map(fun relink_value/1, Arguments) };

relink_value(Block=#{ ?VALUE := Values }) when is_list(Values) ->
    io:fwrite("===> ~p~n", [Block]),
    Block#{ ?VALUE => lists:map(fun relink_value/1, Values) };

%% No relinking
relink_value(Value) ->
    Value.
