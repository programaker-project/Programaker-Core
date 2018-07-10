-module(automate_bot_engine_naive_lists).


-export([ remove_nth/2
        , insert_nth/3
        , replace_nth/3
        , get_nth/2
        , get_length/1
        , contains/2
        , get_item_num/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec remove_nth([any()], pos_integer()) -> [any()].
remove_nth(List, Index) when Index > 0 ->
    naive_remove_nth_from_list([], List, Index).

-spec insert_nth([any()], pos_integer(), any()) -> [any(), ...].
insert_nth(List, Index, Value) when Index > 0 ->
    naive_insert_nth_into_list([], List, Index, Value).

-spec replace_nth([any()], pos_integer(), any()) -> [any()].
replace_nth(List, Index, Value) when Index > 0 ->
    naive_replace_nth_into_list([], List, Index, Value).

-spec get_nth([any()], pos_integer()) -> {ok, any()} | {error, not_found}.
get_nth(List, Nth) when is_integer(Nth) and (Nth > 0) ->
    case Nth =< length(List) of
        true ->
            {ok, lists:nth(Nth, List)};
        false ->
            {error, not_found}
    end.

-spec get_length([any()]) -> {ok, non_neg_integer()}.
get_length(List) when is_list(List) ->
    {ok, length(List)}.

-spec contains([any()], any()) -> boolean().
contains(List, Value) ->
    lists:any(fun (E) -> automate_bot_engine_typing:is_equivalent(E, Value) end,
              List).

-spec get_item_num([any()], any()) -> {ok, pos_integer()} | {error, not_found}.
get_item_num(List, Value) ->
    find_item(List, Value, 1).
%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec naive_remove_nth_from_list([any()], maybe_improper_list(), pos_integer()) -> [any()].
naive_remove_nth_from_list(Left, [_Discarded | Right], 1) ->
    lists:reverse(Left) ++ Right;

naive_remove_nth_from_list(Left, [], _) ->
    lists:reverse(Left);

naive_remove_nth_from_list(Left, [Moved | T], ToGo) when ToGo > 0 ->
    naive_remove_nth_from_list([Moved | Left], T, ToGo - 1).

-spec naive_insert_nth_into_list([any()], [any()], pos_integer(), any()) -> nonempty_maybe_improper_list().
naive_insert_nth_into_list(Left, Right, 1, Value) ->
    lists:reverse(Left) ++ [Value | Right];

naive_insert_nth_into_list(Left, [], _, Value)->
    lists:reverse([Value | Left]);

naive_insert_nth_into_list(Left, [Moved | T], ToGo, Value) when ToGo > 0 ->
    naive_insert_nth_into_list([Moved | Left], T, ToGo - 1, Value).

-spec naive_replace_nth_into_list([any()], [any()], pos_integer(), any()) -> maybe_improper_list().
naive_replace_nth_into_list(Left, [_Discarded | Right], 1, Value) ->
    lists:reverse(Left) ++ [Value | Right];

naive_replace_nth_into_list(Left, [], 1, Value)->
    lists:reverse([Value | Left]);

naive_replace_nth_into_list(Left, [], _, _Value)->
    lists:reverse(Left);

naive_replace_nth_into_list(Left, [Moved | T], ToGo, Value) when ToGo > 0 ->
    naive_replace_nth_into_list([Moved | Left], T, ToGo - 1, Value).

-spec find_item(maybe_improper_list(), any(), pos_integer()) -> {ok, pos_integer()} | {error, not_found}.
find_item([Value | _ ], Value, Index) ->
    {ok, Index};

find_item([], _, _) ->
    {error, not_found};

find_item([_ | T ], Value, Index) ->
    find_item(T, Value, Index + 1).


