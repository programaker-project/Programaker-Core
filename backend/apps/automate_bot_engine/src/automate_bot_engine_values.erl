-module(automate_bot_engine_values).

%% API
-export([ add/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================
-spec add(binary(), binary()) -> binary().
add(Previous, Change) when is_binary(Previous) and is_binary(Change) ->
    case combined_type(Previous, Change) of
        {integer, PreviousInt, ChangeInt} ->
            erlang:integer_to_binary(PreviousInt + ChangeInt);
        {float, PreviousF, ChangeF} ->
            erlang:float_to_binary(PreviousF + ChangeF);
        {string, PreviousS, ChangeS} ->
            <<PreviousS/binary, ChangeS/binary>>
    end;

%% If everything else failed, just do simple concatenation
add(V1, V2) ->
    binary:list_to_bin(lists:flatten(io_lib:format("~s~s", [to_string(V1), to_string(V2)]))).

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
                X = {error, _Reason} ->
                    X
            end
    end.
