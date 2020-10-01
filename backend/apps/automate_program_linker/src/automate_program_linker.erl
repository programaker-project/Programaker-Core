-module(automate_program_linker).

-export([ link_program/2
        ]).

-include("../../automate_service_registry/src/records.hrl").
-include("../../automate_bot_engine/src/instructions.hrl").
-include("records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec link_program(program(), owner_id()) -> {ok, program()}.
link_program(Program = #{ <<"blocks">> := Blocks },
             Owner) ->
    RelinkedBlocks = [relink_subprogram(Subprogram, Owner) || Subprogram <- Blocks],
    {ok, Program#{ <<"blocks">> => RelinkedBlocks }}.


relink_subprogram(Subprogram, Owner) ->
    [relink_block(Block, Owner) || Block <- Subprogram].


%% Relink service monitor
relink_block(Block, Owner) ->
    B1 = relink_block_contents(Block, Owner),
    B2 = relink_block_args_values(B1, Owner),
    relink_block_values(B2, Owner).

relink_block_contents(Value = #{ ?TYPE := ?COMMAND_WAIT_FOR_NEXT_VALUE
                               , ?ARGUMENTS := Arguments
                               }, Owner) ->
    io:fwrite("Args: ~p~n", [Arguments]),
    Value#{ ?ARGUMENTS => lists:map(fun(B) -> relink_block(B, Owner) end,
                                    Arguments)
          };

relink_block_contents(Block=#{ ?CONTENTS := Contents
                             }, Owner) when is_list(Contents) ->
    Block#{ ?CONTENTS => lists:map(fun(B) -> relink_block(B, Owner) end,
                                   Contents)
          };

relink_block_contents(Block, _Owner) ->
    Block.


relink_block_args_values(Block=#{ ?ARGUMENTS := Arguments
                                }, _Owner) when is_list(Arguments) ->
    Block#{ ?ARGUMENTS := [ relink_value(Arg) || Arg <- Arguments ] };

relink_block_args_values(Block, _Owner) ->
    Block.

%%%% Relink values
relink_block_values(Block=#{ ?VALUE := Value
                           }, _Owner) when is_list(Value) ->
    Block#{ ?VALUE => lists:map(fun relink_value/1, Value) };

relink_block_values(Block, _Owner) ->
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
relink_value(Block=#{ ?ARGUMENTS := Arguments }) when is_list(Arguments) ->
    Block#{ ?ARGUMENTS => lists:map(fun relink_value/1, Arguments) };

relink_value(Block=#{ ?VALUE := Values }) when is_list(Values) ->
    Block#{ ?VALUE => lists:map(fun relink_value/1, Values) };

%% No relinking
relink_value(Value) ->
    Value.
