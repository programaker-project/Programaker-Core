-module(automate_program_linker).

-export([ link_program/2
        ]).

-include("../../automate_service_registry/src/records.hrl").
-include("../../automate_bot_engine/src/instructions.hrl").
-include("records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec link_program('_', binary()) -> {ok, '_'}.
link_program(Program = #{ <<"blocks">> := Blocks },
             UserId) ->
    io:fwrite("Linking: ~p~n", [Program]),
    RelinkedBlocks = lists:map(fun (Subprogram) -> relink_subprogram(Subprogram, UserId) end, Blocks),
    {ok, Program#{ <<"blocks">> => RelinkedBlocks }}.


relink_subprogram(Subprogram, UserId) ->
    lists:map(fun (Block) -> relink_block(Block, UserId) end, Subprogram).

%% Relink chat_say
relink_block(Block=#{ ?TYPE := <<"chat_say">>, ?ARGUMENTS := Arguments }, _UserId) ->
    Block#{ ?TYPE := ?COMMAND_CALL_SERVICE
          , ?ARGUMENTS := #{ ?SERVICE_ACTION => send_chat
                           , ?SERVICE_ID => automate_services_telegram:get_uuid()
                           , ?SERVICE_CALL_VALUES => Arguments
                           }
          };

%% Relink service monitor
relink_block(Block, UserId) ->
    relink_block_args(Block, UserId).

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
