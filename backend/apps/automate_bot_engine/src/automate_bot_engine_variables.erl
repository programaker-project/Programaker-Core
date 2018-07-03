-module(automate_bot_engine_variables).

%% API
-export([ resolve_argument/1
        , resolve_argument/2

        , set_thread_value/3
        , get_program_variable/2
        , set_program_variable/3

        , retrieve_thread_value/2
        , retrieve_thread_values/2

        , retrieve_instruction_memory/1
        , set_instruction_memory/2
        , unset_instruction_memory/1
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec resolve_argument(map()) -> {ok, any()} | {error, not_found}.
resolve_argument(#{ ?TYPE := ?VARIABLE_CONSTANT
                  , ?VALUE := Value
                  }) ->
    {ok, Value}.

-spec resolve_argument(map(), #program_thread{}) -> {ok, any()} | {error, not_found}.
resolve_argument(#{ ?TYPE := ?VARIABLE_CONSTANT
                  , ?VALUE := Value
                  }, _Thread) ->
    {ok, Value};

resolve_argument(#{ ?TYPE := ?VARIABLE_BLOCK
                  , ?VALUE := [Operator]
                  }, Thread) ->
    automate_bot_engine_operations:get_result(Operator, Thread);

resolve_argument(#{ ?TYPE := ?VARIABLE_VARIABLE
                  , ?VALUE := VariableName
                  }, Thread) ->
    get_program_variable(Thread, VariableName).


-spec retrieve_thread_value(#program_thread{}, atom()) -> {ok, any()} | {error, any()}.
retrieve_thread_value(#program_thread{ global_memory=Global }, Key) ->
    case maps:find(Key, Global) of
        Response = {ok, _} ->
            Response;
        error ->
            {error, not_found}
    end.

-spec retrieve_thread_values(#program_thread{}, [atom()]) -> {ok, [any()]} | {error, any()}.
retrieve_thread_values(Thread, Keys) ->
    retrieve_thread_values(Thread, Keys, []).

-spec set_thread_value(#program_thread{}, atom(), any()) -> {ok, #program_thread{}}.
set_thread_value(Thread = #program_thread{ global_memory=Global }, Key, Value) ->
    {ok, Thread#program_thread{ global_memory=Global#{ Key => Value } } }.

-spec set_program_variable(#program_thread{}, atom(), any()) -> {ok, #program_thread{}}.
set_program_variable(Thread = #program_thread{ program_id=ProgramId }, Key, Value) ->
    ok = automate_storage:set_program_variable(ProgramId, Key, Value),
    {ok, Thread}.

-spec get_program_variable(#program_thread{}, atom()) -> {ok, any()} | {error, not_}.
get_program_variable(#program_thread{ program_id=ProgramId }, Key) ->
    automate_storage:get_program_variable(ProgramId, Key).

-spec retrieve_instruction_memory(#program_thread{}) -> {ok, any()} | {error, not_found}.
retrieve_instruction_memory(#program_thread{ instruction_memory=Memory, position=Position }) ->
    case maps:find(Position, Memory) of
        Response = {ok, _} ->
            Response;
        error ->
            {error, not_found}
    end.

-spec set_instruction_memory(#program_thread{}, any()) -> #program_thread{}.
set_instruction_memory(Thread=#program_thread{ instruction_memory=Memory, position=Position }, Value) ->
    Thread#program_thread{ instruction_memory=Memory#{ Position => Value } }.

-spec unset_instruction_memory(#program_thread{}) -> #program_thread{}.
unset_instruction_memory(Thread=#program_thread{ instruction_memory=Memory, position=Position }) ->
    Thread#program_thread{ instruction_memory=maps:remove(Position, Memory) }.

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
