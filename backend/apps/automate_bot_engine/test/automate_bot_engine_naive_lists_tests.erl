%%% @doc
%%% Automate bot engine list implementation tests.
%%% @end

-module(automate_bot_engine_naive_lists_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test API
%%====================================================================

session_manager_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

setup() ->
    ok.

stop(ok) ->
    ok.

tests(_SetupResult) ->
    %% Removal
    [ {"[Naive lists][Remove] Remove non existing", fun remove_non_existing/0}
    , {"[Naive lists][Remove] Remove first of one", fun remove_first_of_one/0}
    , {"[Naive lists][Remove] Remove first of none", fun remove_first_of_none/0}
    , {"[Naive lists][Remove] Remove first of many", fun remove_first_of_five/0}
    , {"[Naive lists][Remove] Remove second of one", fun remove_second_of_one/0}
    , {"[Naive lists][Remove] Remove second of none", fun remove_second_of_none/0}
    , {"[Naive lists][Remove] Remove second of two", fun remove_second_of_two/0}
    , {"[Naive lists][Remove] Remove second of three", fun remove_second_of_three/0}
    , {"[Naive lists][Remove] Remove second of four", fun remove_second_of_four/0}
    , {"[Naive lists][Remove] Remove second of five", fun remove_second_of_five/0}
    , {"[Naive lists][Remove] Remove third of one", fun remove_third_of_one/0}
    , {"[Naive lists][Remove] Remove third of none", fun remove_third_of_none/0}
    , {"[Naive lists][Remove] Remove third of two", fun remove_third_of_two/0}
    , {"[Naive lists][Remove] Remove third of three", fun remove_third_of_three/0}
    , {"[Naive lists][Remove] Remove third of four", fun remove_third_of_four/0}
    , {"[Naive lists][Remove] Remove third of five", fun remove_third_of_five/0}

      %% Insertion
    , {"[Naive lists][Insert] Insert in non existing", fun insert_in_non_existing/0}
    , {"[Naive lists][Insert] Insert in first of one", fun insert_in_first_of_one/0}
    , {"[Naive lists][Insert] Insert in first of none", fun insert_in_first_of_none/0}
    , {"[Naive lists][Insert] Insert in first of many", fun insert_in_first_of_five/0}
    , {"[Naive lists][Insert] Insert in second of one", fun insert_in_second_of_one/0}
    , {"[Naive lists][Insert] Insert in second of none", fun insert_in_second_of_none/0}
    , {"[Naive lists][Insert] Insert in second of two", fun insert_in_second_of_two/0}
    , {"[Naive lists][Insert] Insert in second of three", fun insert_in_second_of_three/0}
    , {"[Naive lists][Insert] Insert in second of four", fun insert_in_second_of_four/0}
    , {"[Naive lists][Insert] Insert in second of five", fun insert_in_second_of_five/0}
    , {"[Naive lists][Insert] Insert in third of one", fun insert_in_third_of_one/0}
    , {"[Naive lists][Insert] Insert in third of none", fun insert_in_third_of_none/0}
    , {"[Naive lists][Insert] Insert in third of two", fun insert_in_third_of_two/0}
    , {"[Naive lists][Insert] Insert in third of three", fun insert_in_third_of_three/0}
    , {"[Naive lists][Insert] Insert in third of four", fun insert_in_third_of_four/0}
    , {"[Naive lists][Insert] Insert in third of five", fun insert_in_third_of_five/0}

      %% Replacement
    , {"[Naive lists][Replace] Replace in non existing", fun replace_in_non_existing/0}
    , {"[Naive lists][Replace] Replace in first of one", fun replace_in_first_of_one/0}
    , {"[Naive lists][Replace] Replace in first of none", fun replace_in_first_of_none/0}
    , {"[Naive lists][Replace] Replace in first of many", fun replace_in_first_of_five/0}
    , {"[Naive lists][Replace] Replace in second of one", fun replace_in_second_of_one/0}
    , {"[Naive lists][Replace] Replace in second of none", fun replace_in_second_of_none/0}
    , {"[Naive lists][Replace] Replace in second of two", fun replace_in_second_of_two/0}
    , {"[Naive lists][Replace] Replace in second of three", fun replace_in_second_of_three/0}
    , {"[Naive lists][Replace] Replace in second of four", fun replace_in_second_of_four/0}
    , {"[Naive lists][Replace] Replace in second of five", fun replace_in_second_of_five/0}
    , {"[Naive lists][Replace] Replace in third of one", fun replace_in_third_of_one/0}
    , {"[Naive lists][Replace] Replace in third of none", fun replace_in_third_of_none/0}
    , {"[Naive lists][Replace] Replace in third of two", fun replace_in_third_of_two/0}
    , {"[Naive lists][Replace] Replace in third of three", fun replace_in_third_of_three/0}
    , {"[Naive lists][Replace] Replace in third of four", fun replace_in_third_of_four/0}
    , {"[Naive lists][Replace] Replace in third of five", fun replace_in_third_of_five/0}

      %% Get item
    , {"[Naive lists][Get item] Get item in non existing", fun get_item_non_existing/0}
    , {"[Naive lists][Get item] Get item in first of one", fun get_item_first_of_one/0}
    , {"[Naive lists][Get item] Get item in first of none", fun get_item_first_of_none/0}
    , {"[Naive lists][Get item] Get item in first of many", fun get_item_first_of_five/0}
    , {"[Naive lists][Get item] Get item in second of one", fun get_item_second_of_one/0}
    , {"[Naive lists][Get item] Get item in second of none", fun get_item_second_of_none/0}
    , {"[Naive lists][Get item] Get item in second of two", fun get_item_second_of_two/0}
    , {"[Naive lists][Get item] Get item in second of three", fun get_item_second_of_three/0}
    , {"[Naive lists][Get item] Get item in second of four", fun get_item_second_of_four/0}
    , {"[Naive lists][Get item] Get item in second of five", fun get_item_second_of_five/0}
    , {"[Naive lists][Get item] Get item in third of one", fun get_item_third_of_one/0}
    , {"[Naive lists][Get item] Get item in third of none", fun get_item_third_of_none/0}
    , {"[Naive lists][Get item] Get item in third of two", fun get_item_third_of_two/0}
    , {"[Naive lists][Get item] Get item in third of three", fun get_item_third_of_three/0}
    , {"[Naive lists][Get item] Get item in third of four", fun get_item_third_of_four/0}
    , {"[Naive lists][Get item] Get item in third of five", fun get_item_third_of_five/0}

      %% Get length
    , {"[Naive lists][Length] Length of empty", fun length_empty/0}
    , {"[Naive lists][Length] Length of one", fun length_one/0}
    , {"[Naive lists][Length] Length of two", fun length_two/0}
    , {"[Naive lists][Length] Length of five", fun length_five/0}

      %% Contains
    , {"[Naive lists][Contains] Not contains empty", fun not_contains_empty/0}
    , {"[Naive lists][Contains] Contains first", fun contains_first/0}
    , {"[Naive lists][Contains] Not contains first", fun not_contains_one/0}
    , {"[Naive lists][Contains] Contains second", fun contains_second/0}
    , {"[Naive lists][Contains] Contains fifth", fun contains_fifth/0}
    , {"[Naive lists][Contains] Not contains fifth", fun not_contains_five/0}

      %% Find
    , {"[Naive lists][Find] Not find empty", fun not_find_empty/0}
    , {"[Naive lists][Find] Find first", fun find_first/0}
    , {"[Naive lists][Find] Not find first", fun not_find_one/0}
    , {"[Naive lists][Find] Find second", fun find_second/0}
    , {"[Naive lists][Find] Find fifth", fun find_fifth/0}
    , {"[Naive lists][Find] Not find fifth", fun not_find_five/0}
    ].

%%%% Removal
remove_non_existing() ->
    [] = automate_bot_engine_naive_lists:remove_nth([], 5).

remove_first_of_one() ->
    [] = automate_bot_engine_naive_lists:remove_nth([5], 1).

remove_first_of_none() ->
    [] = automate_bot_engine_naive_lists:remove_nth([], 1).

remove_first_of_five() ->
    [4, 3, 2, 1] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3, 2, 1], 1).

remove_second_of_none() ->
    [] = automate_bot_engine_naive_lists:remove_nth([], 2).

remove_second_of_one() ->
    [5] = automate_bot_engine_naive_lists:remove_nth([5], 2).

remove_second_of_two() ->
    [5] = automate_bot_engine_naive_lists:remove_nth([5, 4], 2).

remove_second_of_three() ->
    [5, 3] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3], 2).

remove_second_of_four() ->
    [5, 3, 2] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3, 2], 2).

remove_second_of_five() ->
    [5, 3, 2, 1] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3, 2, 1], 2).

remove_third_of_none() ->
    [] = automate_bot_engine_naive_lists:remove_nth([], 3).

remove_third_of_one() ->
    [5] = automate_bot_engine_naive_lists:remove_nth([5], 3).

remove_third_of_two() ->
    [5, 4] = automate_bot_engine_naive_lists:remove_nth([5, 4], 3).

remove_third_of_three() ->
    [5, 4] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3], 3).

remove_third_of_four() ->
    [5, 4, 2] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3, 2], 3).

remove_third_of_five() ->
    [5, 4, 2, 1] = automate_bot_engine_naive_lists:remove_nth([5, 4, 3, 2, 1], 3).

%%%% Insertion
insert_in_non_existing() ->
    [inserted] = automate_bot_engine_naive_lists:insert_nth([], 1, inserted).

insert_in_first_of_one() ->
    [inserted, 5] = automate_bot_engine_naive_lists:insert_nth([5], 1, inserted).

insert_in_first_of_none() ->
    [inserted] = automate_bot_engine_naive_lists:insert_nth([], 1, inserted).

insert_in_first_of_five() ->
    [inserted, 5, 4, 3, 2, 1] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3, 2, 1], 1, inserted).

insert_in_second_of_none() ->
    [inserted] = automate_bot_engine_naive_lists:insert_nth([], 2, inserted).

insert_in_second_of_one() ->
    [5, inserted] = automate_bot_engine_naive_lists:insert_nth([5], 2, inserted).

insert_in_second_of_two() ->
    [5, inserted, 4] = automate_bot_engine_naive_lists:insert_nth([5, 4], 2, inserted).

insert_in_second_of_three() ->
    [5, inserted, 4, 3] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3], 2, inserted).

insert_in_second_of_four() ->
    [5, inserted, 4, 3, 2] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3, 2], 2, inserted).

insert_in_second_of_five() ->
    [5, inserted, 4, 3, 2, 1] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3, 2, 1], 2, inserted).

insert_in_third_of_none() ->
    [inserted] = automate_bot_engine_naive_lists:insert_nth([], 3, inserted).

insert_in_third_of_one() ->
    [5, inserted] = automate_bot_engine_naive_lists:insert_nth([5], 3, inserted).

insert_in_third_of_two() ->
    [5, 4, inserted] = automate_bot_engine_naive_lists:insert_nth([5, 4], 3, inserted).

insert_in_third_of_three() ->
    [5, 4, inserted, 3] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3], 3, inserted).

insert_in_third_of_four() ->
    [5, 4, inserted, 3, 2] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3, 2], 3, inserted).

insert_in_third_of_five() ->
    [5, 4, inserted, 3, 2, 1] = automate_bot_engine_naive_lists:insert_nth([5, 4, 3, 2, 1], 3, inserted).


%%%% Replacement
replace_in_non_existing() ->
    [replaced] = automate_bot_engine_naive_lists:replace_nth([], 1, replaced).

replace_in_first_of_one() ->
    [replaced] = automate_bot_engine_naive_lists:replace_nth([5], 1, replaced).

replace_in_first_of_none() ->
    [replaced] = automate_bot_engine_naive_lists:replace_nth([], 1, replaced).

replace_in_first_of_five() ->
    [replaced, 4, 3, 2, 1] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3, 2, 1], 1, replaced).

replace_in_second_of_none() ->
    [] = automate_bot_engine_naive_lists:replace_nth([], 2, replaced).

replace_in_second_of_one() ->
    [5, replaced] = automate_bot_engine_naive_lists:replace_nth([5], 2, replaced).

replace_in_second_of_two() ->
    [5, replaced] = automate_bot_engine_naive_lists:replace_nth([5, 4], 2, replaced).

replace_in_second_of_three() ->
    [5, replaced, 3] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3], 2, replaced).

replace_in_second_of_four() ->
    [5, replaced, 3, 2] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3, 2], 2, replaced).

replace_in_second_of_five() ->
    [5, replaced, 3, 2, 1] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3, 2, 1], 2, replaced).

replace_in_third_of_none() ->
    [] = automate_bot_engine_naive_lists:replace_nth([], 3, replaced).

replace_in_third_of_one() ->
    [5] = automate_bot_engine_naive_lists:replace_nth([5], 3, replaced).

replace_in_third_of_two() ->
    [5, 4, replaced] = automate_bot_engine_naive_lists:replace_nth([5, 4], 3, replaced).

replace_in_third_of_three() ->
    [5, 4, replaced] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3], 3, replaced).

replace_in_third_of_four() ->
    [5, 4, replaced, 2] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3, 2], 3, replaced).

replace_in_third_of_five() ->
    [5, 4, replaced, 2, 1] = automate_bot_engine_naive_lists:replace_nth([5, 4, 3, 2, 1], 3, replaced).


%%%% Get item
get_item_non_existing() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([], 1).

get_item_first_of_one() ->
    {ok, 5} = automate_bot_engine_naive_lists:get_nth([5], 1).

get_item_first_of_none() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([], 1).

get_item_first_of_five() ->
    {ok, 5} = automate_bot_engine_naive_lists:get_nth([5, 4, 3, 2, 1], 1).

get_item_second_of_none() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([], 2).

get_item_second_of_one() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([5], 2).

get_item_second_of_two() ->
    {ok, 4} = automate_bot_engine_naive_lists:get_nth([5, 4], 2).

get_item_second_of_three() ->
    {ok, 4} = automate_bot_engine_naive_lists:get_nth([5, 4, 3], 2).

get_item_second_of_four() ->
    {ok, 4} = automate_bot_engine_naive_lists:get_nth([5, 4, 3, 2], 2).

get_item_second_of_five() ->
    {ok, 4} = automate_bot_engine_naive_lists:get_nth([5, 4, 3, 2, 1], 2).

get_item_third_of_none() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([], 3).

get_item_third_of_one() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([5], 3).

get_item_third_of_two() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_nth([5, 4], 3).

get_item_third_of_three() ->
    {ok, 3} = automate_bot_engine_naive_lists:get_nth([5, 4, 3], 3).

get_item_third_of_four() ->
    {ok, 3} = automate_bot_engine_naive_lists:get_nth([5, 4, 3, 2], 3).

get_item_third_of_five() ->
    {ok, 3} = automate_bot_engine_naive_lists:get_nth([5, 4, 3, 2, 1], 3).

%% Length
length_empty() ->
    {ok, 0} = automate_bot_engine_naive_lists:get_length([]).

length_one() ->
    {ok, 1} = automate_bot_engine_naive_lists:get_length([5]).

length_two() ->
    {ok, 2} = automate_bot_engine_naive_lists:get_length([5, 4]).

length_five() ->
    {ok, 5} = automate_bot_engine_naive_lists:get_length([5, 4, 3, 2, 1]).

%% Containing
not_contains_empty() ->
    false = automate_bot_engine_naive_lists:contains([], fake).

contains_first() ->
    true = automate_bot_engine_naive_lists:contains([5], 5).

not_contains_one() ->
    false = automate_bot_engine_naive_lists:contains([5], fake).

contains_second() ->
    true = automate_bot_engine_naive_lists:contains([5, 4, 3, 2, 1], 4).

contains_fifth() ->
    true = automate_bot_engine_naive_lists:contains([5, 4, 3, 2, 1], 1).

not_contains_five() ->
    false = automate_bot_engine_naive_lists:contains([5, 4, 3, 2, 1], fake).

%% Containing
not_find_empty() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_item_num([], fake).

find_first() ->
    {ok, 1} = automate_bot_engine_naive_lists:get_item_num([5], 5).

not_find_one() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_item_num([5], fake).

find_second() ->
    {ok, 2} = automate_bot_engine_naive_lists:get_item_num([5, 4, 3, 2, 1], 4).

find_fifth() ->
    {ok, 5} = automate_bot_engine_naive_lists:get_item_num([5, 4, 3, 2, 1], 1).

not_find_five() ->
    {error, not_found} = automate_bot_engine_naive_lists:get_item_num([5, 4, 3, 2, 1], fake).
