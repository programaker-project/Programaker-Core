-module(automate_bot_engine_values).

%% API
-export([ add/2
        , join/2
        , get_value_by_key/2
        , subtract/2
        , multiply/2
        , divide/2
        , modulo/2
        , is_less_than/2
        , is_greater_than/2
        , are_equal/1
        , is_equal_to/2
        , string_contains/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================
-spec add(_, _) -> {ok, number()}.
add(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, PreviousInt + ChangeInt};
        {float, PreviousF, ChangeF} ->
            {ok, PreviousF + ChangeF};
        {string, LeftS, RightS} ->
            { ok, binary:list_to_bin(io_lib:format("~s~s", [LeftS, RightS])) }
    end;

%% If everything else failed, just do simple concatenation
add(V1, V2) ->
    add(to_bin(V1), to_bin(V2)).

-spec join(_, _) -> {ok, binary()} | {error, any()}.
join(V1, V2) ->
    {ok, binary:list_to_bin(lists:flatten(io_lib:format("~s~s", [to_string(V1), to_string(V2)])))}.

-spec get_value_by_key(binary(), map()) -> {ok, binary()} | {error, not_found}.
get_value_by_key(Key, Map) when is_map(Map) and is_binary(Key) ->
    case maps:is_key(Key, Map) of
        true -> {ok, maps:get(Key, Map)};
        false -> {error, not_found}
    end;

%% If this is not a map, fail
get_value_by_key(_V1, _V2) ->
    {error, not_found}.

-spec subtract(_, _) -> {ok, number()} | {error, not_found}.
subtract(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, PreviousInt - ChangeInt};
        {float, PreviousF, ChangeF} ->
            {ok, PreviousF - ChangeF};
        _ ->
            {error, not_found}
    end;

subtract(V1, V2) ->
    subtract(to_bin(V1), to_bin(V2)).


-spec multiply(_, _) -> {ok, number()} | {error, not_found}.
multiply(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, PreviousInt * ChangeInt};
        {float, PreviousF, ChangeF} ->
            {ok, PreviousF * ChangeF};
        _ ->
            {error, not_found}
    end;

multiply(V1, V2) ->
    multiply(to_bin(V1), to_bin(V2)).


-spec divide(_, _) -> {ok, number()} | {error, not_found}.
divide(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, PreviousInt / ChangeInt};
        {float, PreviousF, ChangeF} ->
            {ok, PreviousF / ChangeF};
        _ ->
            {error, not_found}
    end;

divide(V1, V2) ->
    divide(to_bin(V1), to_bin(V2)).

-spec modulo(_, _) -> {ok, number()} | {error, not_found}.
modulo(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            %% Probably overkill, but this implements a proper modulo, and not
            %% just a truncated division.
            {ok, math:fmod(math:fmod(PreviousInt, ChangeInt) + ChangeInt, ChangeInt)};
        {float, PreviousF, ChangeF} ->
            {ok, math:fmod(math:fmod(PreviousF, ChangeF) + ChangeF, ChangeF)};
        _ ->
            {error, not_found}
    end;

modulo(V1, V2) ->
    modulo(to_bin(V1), to_bin(V2)).


-spec is_less_than(_, _) -> {ok, boolean()} | {error, not_found}.
is_less_than(V1, V2) when is_binary(V1) and is_binary(V2) ->
    case combined_type(V1, V2) of
        {integer, Int1, Int2} ->
            {ok, Int1 < Int2};
        {float, Float1, Float2} ->
            {ok, Float1 < Float2};
        {string, String1, String2} ->
            {ok, String1 < String2}
    end;

is_less_than(V1, V2) ->
    is_less_than(to_bin(V1), to_bin(V2)).

-spec is_greater_than(_, _) -> {ok, boolean()} | {error, not_found}.
is_greater_than(V1, V2) when is_binary(V1) and is_binary(V2) ->
    case combined_type(V1, V2) of
        {integer, Int1, Int2} ->
            {ok, Int1 > Int2};
        {float, Float1, Float2} ->
            {ok, Float1 > Float2};
        {string, String1, String2} ->
            {ok, String1 > String2}
    end;

is_greater_than(V1, V2) ->
    is_greater_than(to_bin(V1), to_bin(V2)).


%% Probably this can be optimized to perform combined_type/2 less times, but
%% this should work for now.

%% NOTE there might be some cases where evaluating combined_type/2 independently
%% might cause strange comparation bugs to happen. If one is found, strongly
%% consider rethinking this function.
-spec are_equal([any]) -> {ok, boolean()}.
are_equal(Values=[_|T]) ->
    Pairs = lists:zip(lists:droplast(Values), T),
    { ok
    , lists:all(fun({X, Y}) ->
                        {ok, Result} = is_equal_to(X, Y),
                        Result
                end, Pairs)
    }.

-spec is_equal_to(_, _) -> {ok, boolean()} | {error, not_found}.
is_equal_to(V1, V2) when is_binary(V1) and is_binary(V2) ->
    case combined_type(V1, V2) of
        {integer, Int1, Int2} ->
            {ok, Int1 == Int2};
        {float, Float1, Float2} ->
            {ok, Float1 == Float2};
        {string, String1, String2} ->
            {ok, String1 == String2}
    end;

is_equal_to(V1, V2) ->
    is_equal_to(to_bin(V1), to_bin(V2)).

-spec string_contains(_, _) -> {ok, boolean()} | {error, not_found}.
string_contains(Haystack, Needle) when is_binary(Haystack) and is_binary(Needle) ->
    CHaystack = canonicalize_string(Haystack),
    CNeedle = canonicalize_string(Needle),
    Result = string:find(CHaystack, CNeedle) =/= nomatch,
    {ok, Result};
string_contains(Haystack, Needle) ->
    string_contains(to_bin(Haystack), to_bin(Needle)).


%%%===================================================================
%%% Type handling methods
%%%===================================================================
to_bin(V) when is_binary(V) ->
    V;
to_bin(V) ->
    binary:list_to_bin(to_string(V)).

to_string(V) when is_binary(V) ->
    V;
to_string(V) when is_list(V) ->
    V;
to_string(V) ->
    io_lib:format("~p", [V]).

combined_type(V1, V2) when is_binary(V1) and is_binary(V2) ->
    case [to_int(V1), to_int(V2)] of
        [{ok, Int1}, {ok, Int2}] ->
            {integer, Int1, Int2};
        _ ->
            case [to_float(V1), to_float(V2)] of
                [{ok, F1}, {ok, F2}] ->
                    {float, F1, F2};
                _ ->
                    {string, V1, V2}
            end
    end;

%% Map all to strings if they are not already
combined_type(V1, V2) ->
    {string, to_bin(V1), to_bin(V2)}.

to_int(Value) when is_binary(Value) ->
    case string:to_integer(Value) of
        {Int, <<"">>} when is_integer(Int) ->
            {ok, Int};
        {_Int, _} when is_integer(_Int) ->
            {error, no_total_match};
        X = {error, _Reason} ->
            X
    end.

to_float(Value) when is_binary(Value) ->
    case string:to_float(Value) of
        {F, <<"">>} when is_float(F) ->
            {ok, F};
        {error, _} ->
            case string:to_integer(Value) of
                {Int, <<"">>} when is_integer(Int) ->
                    {ok, erlang:float(Int)};
                {_Int, _} when is_integer(_Int) ->
                    {error, no_total_match};
                X = {error, _Reason} ->
                    X
            end
    end.

%% Convert non-ascii characters to their closest ones.
canonicalize_string(Str) when is_binary(Str) ->

    Uni = unicode:characters_to_binary(Str, utf8), % This corrects non-unicode binaries. For example the ones containing 'Ã³' pairs instead of 'ó'
    Decomposed = unicode:characters_to_nfkd_list(Uni),
    Filtered = lists:filter(fun(X) -> X < 256 end, Decomposed), % Remove non-ascii characters

    %% Unify casing and convert back from list to binary.
    list_to_binary(string:casefold(Filtered)).
