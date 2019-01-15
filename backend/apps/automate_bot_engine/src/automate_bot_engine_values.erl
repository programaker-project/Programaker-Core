-module(automate_bot_engine_values).

%% API
-export([ add/2
        , subtract/2
        , multiply/2
        , divide/2
        , is_less_than/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================
-spec add(binary(), binary()) -> {ok, binary()}.
add(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, erlang:integer_to_binary(PreviousInt + ChangeInt)};
        {float, PreviousF, ChangeF} ->
            {ok, erlang:float_to_binary(PreviousF + ChangeF)};
        {string, PreviousS, ChangeS} ->
            {ok, <<PreviousS/binary, ChangeS/binary>>}
    end;

%% If everything else failed, just do simple concatenation
add(V1, V2) ->
    binary:list_to_bin(lists:flatten(io_lib:format("~s~s", [to_string(V1), to_string(V2)]))).

-spec subtract(binary(), binary()) -> {ok, binary()} | {error, not_found}.
subtract(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, erlang:integer_to_binary(PreviousInt - ChangeInt)};
        {float, PreviousF, ChangeF} ->
            {ok, erlang:float_to_binary(PreviousF - ChangeF)};
        _ ->
            {error, not_found}
    end;

subtract(_, _) ->
    {error, not_found}.


-spec multiply(binary(), binary()) -> {ok, binary()} | {error, not_found}.
multiply(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, erlang:integer_to_binary(PreviousInt * ChangeInt)};
        {float, PreviousF, ChangeF} ->
            {ok, erlang:float_to_binary(PreviousF * ChangeF)};
        _ ->
            {error, not_found}
    end;

multiply(_, _) ->
    {error, not_found}.


-spec divide(binary(), binary()) -> {ok, binary()} | {error, not_found}.
divide(Left, Right) when is_binary(Left) and is_binary(Right) ->
    case combined_type(Left, Right) of
        {integer, PreviousInt, ChangeInt} ->
            {ok, erlang:float_to_binary(PreviousInt / ChangeInt)};
        {float, PreviousF, ChangeF} ->
            {ok, erlang:float_to_binary(PreviousF / ChangeF)};
        _ ->
            {error, not_found}
    end;

divide(_, _) ->
    {error, not_found}.


-spec is_less_than(binary(), binary()) -> {ok, binary()} | {error, not_found}.
is_less_than(V1, V2) when is_binary(V1) and is_binary(V2) ->
    case combined_type(V1, V2) of
        {integer, Int1, Int2} ->
            Int1 < Int2;
        {float, Float1, Float2} ->
            Float1 < Float2;
        {string, String1, String2} ->
            String1 < String2
    end;

is_less_than(V1, V2) ->
    {error, not_found}.


%%%===================================================================
%%% Type handling methods
%%%===================================================================
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

%% If everything else failed, just do simple concatenation
combined_type(V1, V2) ->
    {string, io_lib:format("~p", [V1]), io_lib:format("~p", [V2])}.

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
