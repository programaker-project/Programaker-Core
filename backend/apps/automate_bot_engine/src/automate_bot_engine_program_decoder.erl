-module(automate_bot_engine_program_decoder).

%%%% API
%% Exposed functions
-export([ initialize_program/2
        , update_program/2
        , get_bridges_on_program/1
        ]).

-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec initialize_program(binary(), #user_program_entry{}) -> {ok, #program_state{}}.
initialize_program(ProgramId,
                   #user_program_entry
                   { owner=OwnerUserId
                   , program_parsed=Parsed
                   , enabled=Enabled}) ->

    try automate_program_linker:link_program(Parsed, OwnerUserId) of
        {ok, #{ <<"variables">> := Variables
              , <<"blocks">> := Blocks
              }} ->

            { ok
            , #program_state{ program_id=ProgramId
                            , variables=Variables
                            , permissions=#program_permissions{ owner_user_id=OwnerUserId }
                            , triggers=get_triggers(Blocks)
                            , enabled=Enabled
                            }};
        X ->
            automate_logging:log_platform(error, io_lib:format(
                                                   "When decoding ~p returned (unexpected): ~p~n",
                                                   [ ProgramId, X ])),
            {ok, #program_state{ program_id=ProgramId
                               , variables=[]
                               , permissions=#program_permissions{ owner_user_id=OwnerUserId }
                               , triggers=[]
                               , enabled=Enabled
                               }}

    catch ErrNS:Err:StackTrace ->
            io:fwrite("\033[41;37m Error decoding program: ~p \033[0m~n", [{ErrNS, Err, StackTrace}]),
            io:fwrite("------- 8< ---------~n~p~n------- 8< ---------~n", [Parsed]),
            {ok, #program_state{ program_id=ProgramId
                               , variables=[]
                               , permissions=#program_permissions{ owner_user_id=OwnerUserId }
                               , triggers=[]
                               , enabled=Enabled
                               }}
    end
        .

-spec update_program(#program_state{}, #user_program_entry{}) -> {ok, #program_state{}}.
update_program(State,
               #user_program_entry
               { owner=OwnerUserId
               , program_parsed=Parsed
               , enabled=Enabled}) ->
    {ok, #{ <<"variables">> := Variables
          , <<"blocks">> := Blocks
          }} = automate_program_linker:link_program(Parsed, OwnerUserId),
    { ok
    , State#program_state{ variables=Variables %% TODO port old variable values
                         , triggers=get_triggers(Blocks)
                         , enabled=Enabled
                         }}.

-spec get_bridges_on_program(#user_program_entry{}) -> { ok, [binary()] } | {error, not_found}.
get_bridges_on_program(#user_program_entry{ program_parsed=undefined}) ->
    {ok, []};
get_bridges_on_program(#user_program_entry{ owner=OwnerUserId, program_parsed=Parsed}) ->
    {ok, #{ <<"blocks">> := Columns } } = automate_program_linker:link_program(Parsed, OwnerUserId),
    {ok, get_bridges_on_columns(Columns, OwnerUserId)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_triggers([map()]) -> [#program_trigger{}].
get_triggers(Blocks) ->
    [get_trigger(Block) || Block <- Blocks].

-spec get_trigger([any() | any()]) -> #program_trigger{}.
get_trigger([Trigger | Program]) ->
    #program_trigger{ condition=Trigger
                    , subprogram=Program
                    }.

get_bridges_on_columns(Columns, OwnerUserId) ->
    Set = sets:from_list(lists:flatmap(fun(Column) ->
                                               get_bridges_on_column(Column, OwnerUserId)
                                       end, Columns)),
    sets:to_list(Set).

get_bridges_on_column(Column, OwnerUserId) ->
    lists:flatmap(fun(Block) ->
                          get_bridges_on_block(Block, OwnerUserId)
                  end, Column).

get_bridges_on_block(Block, OwnerUserId) ->
    SubBlockBridges = get_subblock_bridges(Block, OwnerUserId),
    ArgBridges = get_argument_bridges(Block, OwnerUserId),
    ValueBridges = get_value_bridges(Block, OwnerUserId),
    case get_bridge_on_block_call(Block, OwnerUserId) of
        {ok, BridgeId} ->
            [BridgeId | SubBlockBridges] ++ ArgBridges ++ ValueBridges;
        {error, not_found} ->
            SubBlockBridges ++ ArgBridges ++ ValueBridges
    end.


get_argument_bridges(#{ ?ARGUMENTS := Arguments } , OwnerUserId) when is_list(Arguments) ->
    lists:flatmap(fun(Arg) -> get_bridges_on_block(Arg, OwnerUserId) end, Arguments);
get_argument_bridges(#{ ?ARGUMENTS := Arguments } , OwnerUserId) when is_map(Arguments) ->
    lists:flatmap(fun({_K, Arg}) -> get_bridges_on_block(Arg, OwnerUserId) end, maps:to_list(Arguments));
get_argument_bridges(_Block, _OwnerUserId) ->
    [].

get_value_bridges(#{ ?VALUE := Values } , OwnerUserId) when is_list(Values) ->
    lists:flatmap(fun(Val) -> get_bridges_on_block(Val, OwnerUserId) end, Values);
get_value_bridges(#{ ?VALUE := Values } , OwnerUserId) when is_map(Values) ->
    lists:flatmap(fun({_K, Val}) -> get_bridges_on_block(Val, OwnerUserId) end, maps:to_list(Values));
get_value_bridges(_Block, _OwnerUserId) ->
    [].


get_subblock_bridges(#{<<"contents">> := Contents}, OwnerUserId) ->
    lists:flatmap(fun (SubBlock) ->
                          get_bridges_on_block(SubBlock, OwnerUserId)
                  end, Contents);
get_subblock_bridges(_, _) ->
    [].


get_bridge_on_block_call(#{ ?TYPE := ?COMMAND_CALL_SERVICE
                          , ?ARGUMENTS := #{ ?SERVICE_ID := ServiceId
                                           }}, OwnerUserId) ->
    service_id_to_bridge_id(ServiceId, OwnerUserId);

get_bridge_on_block_call(#{ ?TYPE := <<"services.", ServiceCall/binary>>
                          }, OwnerUserId) ->
    [ServiceId, _Action] = binary:split(ServiceCall, <<".">>),
   service_id_to_bridge_id(ServiceId, OwnerUserId);

get_bridge_on_block_call(_, _) ->
    {error, not_found}.

service_id_to_bridge_id(ServiceId, _OwnerUserId) ->
    case automate_service_registry:get_service_by_id(ServiceId) of
        {ok, #{ module := Module }} ->
            case Module of
                {automate_service_port_engine_service, [BridgeId]} ->
                    {ok, BridgeId};
                _ ->
                    {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found}
    end.
