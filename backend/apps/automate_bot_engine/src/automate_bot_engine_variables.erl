-module(automate_bot_engine_variables).

%% API
-export([ resolve_argument/1
        , set_thread_value/3
        , set_thread_variable/3
        , retrieve_thread_value/2
        , retrieve_thread_values/2
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

%%%===================================================================
%%% API
%%%===================================================================
resolve_argument(#{ ?TYPE := ?VARIABLE_CONSTANT
                  , ?VALUE := Value
                  }) ->
    {ok, Value}.


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

-spec set_thread_variable(#program_thread{}, atom(), any()) -> {ok, #program_thread{}}.
set_thread_variable(Thread = #program_thread{ variables=Variables }, Key, Value) ->
    {ok, Thread#program_thread{ variables=Variables#{ Key => Value } } }.

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
