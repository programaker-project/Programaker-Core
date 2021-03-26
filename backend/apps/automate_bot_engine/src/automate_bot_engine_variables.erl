-module(automate_bot_engine_variables).

%% API
-export([ resolve_argument/3

        , set_thread_value/3
        , get_program_variable/2
        , set_program_variable/3
        , delete_program_variable/2

        , set_last_bridge_value/3
        , get_last_bridge_value/2

        , retrieve_thread_value/2
        , retrieve_thread_values/2

        , retrieve_instruction_memory/1
        , retrieve_instruction_memory/2
        , set_instruction_memory/2
        , set_instruction_memory/3
        , unset_instruction_memory/1
        , get_thread_context/1
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").
-define(UTILS, automate_bot_engine_utils).

%%%===================================================================
%%% API
%%%===================================================================

%% resolve_argument(#{ ?TYPE := ?VARIABLE_BLOCK
%%                   , ?VALUE := [ #{ ?TYPE := <<"monitor.retrieve.", MonitorId/binary>>
%%                                  }]
%%                   }) ->
%%     automate_monitor_engine:get_last_monitor_result(MonitorId).

-spec resolve_argument(map(), #program_thread{}, map()) -> {ok, any(), #program_thread{}} | {error, not_found}.
resolve_argument(#{ ?TYPE := ?VARIABLE_CONSTANT
                  , ?VALUE := Value
                  }, Thread, _ParentBlock) ->
    {ok, Value, Thread};

resolve_argument(#{ ?TYPE := ?VARIABLE_BLOCK
                  , ?VALUE := [Operator]
                  }, Thread, _ParentBlock) ->
    automate_bot_engine_operations:get_result(Operator, Thread);

resolve_argument(Op=#{ ?TYPE := ?VARIABLE_VARIABLE
                     , ?VALUE := VariableName
                     }, Thread, ParentBlock) ->
    case get_program_variable(Thread, VariableName) of
        {ok, Value} ->
            {ok, Value, Thread};
        {error, not_found} ->
            BlockId = case {?UTILS:get_block_id(Op), ?UTILS:get_block_id(ParentBlock)} of
                          { none, Value } -> Value;
                          { Value, _ } -> Value
                          end,

            throw(#program_error{ error=#variable_not_set{ variable_name=VariableName }
                                , block_id=BlockId
                                })
    end;

resolve_argument(Op=#{ ?TYPE := ?VARIABLE_LIST
                     , ?VALUE := VariableName
                     }, Thread, ParentBlock) ->
    case get_program_variable(Thread, VariableName) of
        {ok, Value} ->
            {ok, Value, Thread};
        {error, not_found} ->
            BlockId = case {?UTILS:get_block_id(Op), ?UTILS:get_block_id(ParentBlock)} of
                          { none, Value } -> Value;
                          { Value, _ } -> Value
                      end,

            throw(#program_error{ error=#list_not_set{ list_name=VariableName }
                                , block_id=BlockId
                                })
    end.

-spec retrieve_thread_value(#program_thread{}, atom() | binary()) -> {ok, any()} | {error, any()}.
retrieve_thread_value(#program_thread{ global_memory=Global }, Key) ->
    case maps:find(Key, Global) of
        Response = {ok, _} ->
            Response;
        error ->
            {error, not_found}
    end.

-spec retrieve_thread_values(#program_thread{}, [atom() | binary()]) -> {ok, [any()]} | {error, any()}.
retrieve_thread_values(Thread, Keys) ->
    retrieve_thread_values(Thread, Keys, []).

-spec set_thread_value(#program_thread{}, binary() | [binary()], any()) -> {ok, #program_thread{}}.
set_thread_value(Thread = #program_thread{}, Key, Value) when is_list(Key) ->
    set_thread_nested_value(Thread, Key, Value);

set_thread_value(Thread = #program_thread{ global_memory=Global }, Key, Value) ->
    {ok, Thread#program_thread{ global_memory=Global#{ Key => Value } } }.

-spec set_program_variable(binary(), binary() | {internal, _}, any()) -> ok | {error, _}.
set_program_variable(ProgramId, Key, Value) ->
    ok = automate_storage:set_program_variable(ProgramId, Key, Value),
    notify_variable_update(Key, ProgramId, Value).

-spec delete_program_variable(binary(), binary()) -> ok | {error, _}.
delete_program_variable(ProgramId, Key) ->
    automate_storage:delete_program_variable(ProgramId, Key).

-spec get_program_variable(#program_thread{} | binary(), binary() | {internal, _}) -> {ok, any()} | {error, not_found}.
get_program_variable(#program_thread{ program_id=ProgramId }, Key) ->
    get_program_variable(ProgramId, Key);
get_program_variable(ProgramId, Key) when is_binary(ProgramId) ->
    case automate_storage:get_program_variable(ProgramId, Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            {error, not_found}
    end.

-spec set_last_bridge_value(#program_thread{}, binary(), any()) -> {ok, #program_thread{}}.
set_last_bridge_value(Thread, BridgeId, Value) ->
    set_thread_value(Thread, [?LAST_BRIDGE_VALUES, BridgeId], Value).

-spec get_last_bridge_value(#program_thread{}, binary()) -> {ok, any()} | {error, not_found}.
get_last_bridge_value(Thread, BridgeId) ->
    get_thread_value(Thread, [?LAST_BRIDGE_VALUES, BridgeId]).

-spec retrieve_instruction_memory(#program_thread{}) -> {ok, any()} | {error, not_found}.
retrieve_instruction_memory(#program_thread{ instruction_memory=Memory, position=Position }) ->
    case maps:find(Position, Memory) of
        Response = {ok, _} ->
            Response;
        error ->
            {error, not_found}
    end.

-spec retrieve_instruction_memory(#program_thread{}, any()) -> {ok, any()} | {error, not_found}.
retrieve_instruction_memory(#program_thread{ instruction_memory=Memory }, Position) ->
    case maps:find(Position, Memory) of
        Response = {ok, _} ->
            Response;
        error ->
            {error, not_found}
    end.

-spec set_instruction_memory(#program_thread{}, any()) -> #program_thread{}.
set_instruction_memory(Thread=#program_thread{ instruction_memory=Memory, position=Position }, Value) ->
    Thread#program_thread{ instruction_memory=Memory#{ Position => Value } }.

-spec set_instruction_memory(#program_thread{}, any(), any()) -> #program_thread{}.
set_instruction_memory(Thread=#program_thread{ instruction_memory=Memory }, Value, Position) ->
    Thread#program_thread{ instruction_memory=Memory#{ Position => Value } }.

-spec unset_instruction_memory(#program_thread{}) -> #program_thread{}.
unset_instruction_memory(Thread=#program_thread{ instruction_memory=Memory, position=Position }) ->
    Thread#program_thread{ instruction_memory=maps:remove(Position, Memory) }.

-spec get_thread_context(#program_thread{}) -> {ok, map()}.
get_thread_context(Thread=#program_thread{ instruction_memory=Memory, position=Position }) ->
    get_context_from_memory(Memory, Position, #{}).

%%%===================================================================
%%% Internal values
%%%===================================================================
retrieve_thread_values(_Thread, [], Acc) ->
    {ok, lists:reverse(Acc)};

retrieve_thread_values(Thread, [Key | T], Acc) ->
    case retrieve_thread_value(Thread, Key) of
        {ok, Value} ->
            retrieve_thread_values(Thread, T, [Value | Acc]);
        Error = {error, _} ->
            Error
    end.

-spec set_thread_nested_value(#program_thread{}, [binary()], any()) -> {ok, #program_thread{}}.
set_thread_nested_value(Thread = #program_thread{ global_memory=Global }, Key, Value) ->
    {ok, Thread#program_thread{ global_memory=set_memory(Key, Value, Global)} }.

-spec set_memory([binary()], any(), map()) -> map().
set_memory([H], V, Mem) ->
    Mem#{ H => V };
set_memory([H | T], V, Mem) ->
    SubMem = case Mem of
                 #{ H := SubValue } ->
                     SubValue;
                 _ ->
                     #{}
             end,
    Mem#{ H => set_memory(T, V, SubMem) }.

-spec get_thread_value(#program_thread{}, [atom()|binary()]) -> {ok, any()} | {error, not_found}.
get_thread_value(#program_thread{ global_memory=Global }, Key) when is_list(Key) ->
    get_memory(Key, Global).

-spec get_memory([atom()|binary()], map()) -> {ok, any()} | {error, not_found}.
get_memory([H], Mem) ->
    case Mem of
        #{ H := V } ->
            {ok, V};
        _ ->
            {error, not_found}
    end;

get_memory([H | T], Mem) ->
    case Mem of
        #{ H := SubMem } ->
            get_memory(T, SubMem);
        _ ->
            {error, not_found}
    end.

get_context_from_memory(_Memory, [], Acc) ->
    {ok, Acc};
get_context_from_memory(Memory, Position, Acc) ->
    InstructionMemory = case maps:find(Position, Memory) of
                            {ok, Result} ->
                                Result;
                            error ->
                                []
                        end,
    get_context_from_memory(Memory, lists:droplast(Position), add_to_context_acc(InstructionMemory, Acc)).

add_to_context_acc([], Context) ->
    Context;
add_to_context_acc([{ context_group, Key, { SubKey, SubValue } } | T], Context) ->
    PrevValue = case maps:is_key(Key, Context) of
                    true -> maps:get(Key, Context);
                    false -> #{}
                end,
    case maps:is_key(SubKey, PrevValue) of
        true ->
            %% Repeated key, as we're going bottom-up, we don't update
            %% the context. This way we get the innermost values, which are
            %% the ones to be used.
            add_to_context_acc(T, Context);
        false ->
            add_to_context_acc(T, Context#{ Key => PrevValue#{ SubKey => SubValue } })
    end;
add_to_context_acc([{ context, Key, Value } | T], Context) ->
    case maps:is_key(Key, Context) of
        true ->
            %% Repeated key, as we're going bottom-up, we don't update
            %% the context. This way we get the innermost values, which are
            %% the ones to be used.
            add_to_context_acc(T, Context);
        false ->
            add_to_context_acc(T, Context#{ Key => Value })
    end;
add_to_context_acc([ _ | T ], Context) ->
    add_to_context_acc(T, Context);
add_to_context_acc(_, Context) ->
    Context.

-spec notify_variable_update(VariableName :: binary() | { internal, _ }, binary(), _) -> ok | {error, _}.
notify_variable_update({internal, _ }, _ProgramId, _Value) ->
    ok; %% Unused at this point
notify_variable_update(VariableName, ProgramId, Value) ->
    case automate_storage:get_program_from_id(ProgramId) of
        {ok, #user_program_entry{ program_channel=ChannelId }} ->
            automate_channel_engine:send_to_channel(ChannelId, #{ <<"key">> => variable_events
                                                                  %% This canonicalization is done also on the channel engine, but it's not saved to the subkey
                                                                , <<"subkey">> => automate_channel_engine_utils:canonicalize_selector(VariableName)
                                                                , <<"value">> => Value
                                                                });
        {error, Reason} ->
            {error, Reason}
    end.
