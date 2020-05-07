%%% Automate bot engine getters tests.
%%% @end

-module(automate_bot_engine_list_operations_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("single_line_program.hrl").

-define(APPLICATION, automate_bot_engine).
-define(TEST_NODES, [node()]).
-define(TEST_MONITOR, <<"__test_monitor__">>).
-define(TEST_SERVICE, automate_service_registry_test_service:get_uuid()).
-define(TEST_SERVICE_ACTION, test_action).

%%====================================================================
%% Test API
%%====================================================================

session_manager_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

%% @doc App infrastructure setup.
%% @end
setup() ->
    NodeName = node(),

    %% %% Use a custom node name to avoid overwriting the actual databases
    %% net_kernel:start([testing, shortnames]),

    %% {ok, Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][List Operations] Add to list (had values)",                       fun add_to_list_with_values/0}
    , {"[Bot engine][List Operations] Add to list (not defined)",                      fun add_to_list_not_defined/0}
    , {"[Bot engine][List Operations] Delete of list",                                 fun delete_of_list/0}
    , {"[Bot engine][List Operations] Delete all list (had values)",                   fun delete_all_list_with_values/0}
    , {"[Bot engine][List Operations] Delete all list (not defined)",                  fun delete_all_list_not_defined/0}
    , {"[Bot engine][List Operations] Insert at position in list (had values)",        fun insert_at_position_in_list_with_values/0}
    , {"[Bot engine][List Operations] Insert at position in list (not defined)",       fun insert_at_position_in_list_not_defined/0}
    , {"[Bot engine][List Operations] Replace item of list (had values)",              fun replace_item_of_list_with_values/0}
    , {"[Bot engine][List Operations] Replace item of list (not defined)",             fun replace_item_of_list_not_defined/0}
    , {"[Bot engine][List Operations] Get item of list (has values, found)",           fun get_item_of_list_with_values_found/0}
    , {"[Bot engine][List Operations] Get item of list (has values, not found)",       fun get_item_of_list_with_values_not_found/0}
    , {"[Bot engine][List Operations] Get item of list (not defined)",                 fun get_item_of_list_not_defined/0}
    , {"[Bot engine][List Operations] Get item index of list (has values, found)",     fun get_item_index_of_list_with_values_found/0}
    , {"[Bot engine][List Operations] Get item index of list (has values, not found)", fun get_item_index_of_list_with_values_not_found/0}
    , {"[Bot engine][List Operations] Get item index of list (not defined)",           fun get_item_index_of_list_not_defined/0}
    , {"[Bot engine][List Operations] Get length of list (has values)",                fun get_length_of_list_with_values/0}
    , {"[Bot engine][List Operations] Get length of list (not defined)",               fun get_length_of_list_not_defined/0}
    , {"[Bot engine][List Operations] Do list contains item (with values, true)",      fun list_contains_item_with_values_true/0}
    , {"[Bot engine][List Operations] Do list contains item (with values, false)",     fun list_contains_item_with_values_false/0}
    , {"[Bot engine][List Operations] Do list contains item (not defined)",            fun list_contains_item_not_defined/0}
    ].

%%%% Operations
add_to_list_with_values() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    {ran_this_tick, Thread2} = automate_bot_engine_operations:run_instruction(
                                 #{ ?TYPE => ?COMMAND_ADD_TO_LIST
                                  , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                     , ?VALUE => <<"test list">>
                                                     }
                                                  ,  constant_val(<<"e">>)
                                                  ]
                                  }, Thread, {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread2),
    ?assertMatch({ok, [a, b, c, d, <<"e">>], _}, R).

add_to_list_not_defined() ->
    {ran_this_tick, Thread} = automate_bot_engine_operations:run_instruction(
                                #{ ?TYPE => ?COMMAND_ADD_TO_LIST
                                 , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                    , ?VALUE => <<"test list">>
                                                    }
                                                 ,  constant_val(<<"test value">>)
                                                 ]
                                 }, empty_thread(), {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread),
    ?assertMatch({ok, [<<"test value">>], _}, R).

delete_of_list() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    {ran_this_tick, Thread2} = automate_bot_engine_operations:run_instruction(
                                 #{ ?TYPE => ?COMMAND_DELETE_OF_LIST
                                  , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                     , ?VALUE => <<"test list">>
                                                     }
                                                  ,  constant_val(2) %% Keep in mind that arrays are 1-indexed
                                                  ]
                                  }, Thread, {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread2),
    ?assertMatch({ok, [a, c, d], _}, R).

delete_all_list_with_values() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    {ran_this_tick, Thread2} = automate_bot_engine_operations:run_instruction(
                                 #{ ?TYPE => ?COMMAND_DELETE_ALL_LIST
                                  , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                     , ?VALUE => <<"test list">>
                                                     }
                                                  ]
                                  }, Thread, {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread2),
    ?assertMatch({ok, [], _}, R).

delete_all_list_not_defined() ->
    {ran_this_tick, Thread} = automate_bot_engine_operations:run_instruction(
                                #{ ?TYPE => ?COMMAND_DELETE_ALL_LIST
                                 , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                    , ?VALUE => <<"test list">>
                                                    }
                                                 ]
                                 }, empty_thread(), {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread),
    ?assertMatch({ok, [], _}, R).

insert_at_position_in_list_with_values() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    {ran_this_tick, Thread2} = automate_bot_engine_operations:run_instruction(
                                 #{ ?TYPE => ?COMMAND_INSERT_AT_LIST
                                  , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                     , ?VALUE => <<"test list">>
                                                     }
                                                  , constant_val(<<"new">>)
                                                  , constant_val(2)
                                                  ]
                                  }, Thread, {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread2),
    ?assertMatch({ok, [a, <<"new">>, b, c, d], _}, R).

insert_at_position_in_list_not_defined() ->
    {ran_this_tick, Thread} = automate_bot_engine_operations:run_instruction(
                                #{ ?TYPE => ?COMMAND_INSERT_AT_LIST
                                 , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                    , ?VALUE => <<"test list">>
                                                    }
                                                 , constant_val(<<"new">>)
                                                 , constant_val(2)
                                                 ]
                                 }, empty_thread(), {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread),
    ?assertMatch({ok, [?LIST_FILL, <<"new">>], _}, R).


replace_item_of_list_with_values() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    {ran_this_tick, Thread2} = automate_bot_engine_operations:run_instruction(
                                 #{ ?TYPE => ?COMMAND_REPLACE_VALUE_AT_INDEX
                                  , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                     , ?VALUE => <<"test list">>
                                                     }
                                                  , constant_val(2)
                                                  , constant_val(<<"new">>)
                                                  ]
                                  }, Thread, {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread2),
    ?assertMatch({ok, [a, <<"new">>, c, d], _}, R).

replace_item_of_list_not_defined() ->
    {ran_this_tick, Thread} = automate_bot_engine_operations:run_instruction(
                                #{ ?TYPE => ?COMMAND_REPLACE_VALUE_AT_INDEX
                                 , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                    , ?VALUE => <<"test list">>
                                                    }
                                                 , constant_val(2)
                                                 , constant_val(<<"new">>)
                                                 ]
                                 }, empty_thread(), {?SIGNAL_PROGRAM_TICK, test}),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_GET_CONTENTS
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread),
    ?assertMatch({ok, [?LIST_FILL, <<"new">>], _}, R).

get_item_of_list_with_values_found() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_ITEM_OF_LIST
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ,  constant_val(2)
                           ]
           }, Thread),
    ?assertMatch({ok, b, _}, R).

get_item_of_list_with_values_not_found() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c, d]),
    ok = try automate_bot_engine_operations:get_result(
               #{ ?TYPE => ?COMMAND_ITEM_OF_LIST
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                   , ?VALUE => <<"test list">>
                                   }
                                ,  constant_val(99)
                                ]
                }, Thread)
         of
             {ok, _, _} -> this_should_fail
         catch throw:Error:_StackTrace ->
                 ?assertMatch(#program_error{error=#index_not_in_list{list_name= <<"test list">>}},
                              Error),
                 ok
         end.

get_item_of_list_not_defined() ->
    Thread = empty_thread(),
    ok = try automate_bot_engine_operations:get_result(
               #{ ?TYPE => ?COMMAND_ITEM_OF_LIST
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                   , ?VALUE => <<"test list">>
                                   }
                                ,  constant_val(99)
                                ]
                }, Thread)
         of
             {ok, _, _} -> this_should_fail
         catch throw:Error:_StackTrace ->
                 ?assertMatch(#program_error{error=#list_not_set{list_name= <<"test list">>}},
                              Error),
                 ok
         end.

get_item_index_of_list_with_values_found() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [<<"a">>, <<"b">>, <<"c">>]),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_ITEMNUM_OF_LIST
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ,  constant_val(<<"c">>)
                           ]
           }, Thread),
    ?assertMatch({ok, 3, _}, R).

get_item_index_of_list_with_values_not_found() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [<<"a">>, <<"b">>, <<"c">>]),
    R = automate_bot_engine_operations:get_result(
               #{ ?TYPE => ?COMMAND_ITEMNUM_OF_LIST
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                   , ?VALUE => <<"test list">>
                                   }
                                ,  constant_val(<<"zzzz">>)
                                ]
                }, Thread),
    ?assertMatch({error, not_found}, R).

get_item_index_of_list_not_defined() ->
    Thread = empty_thread(),
    ok = try automate_bot_engine_operations:get_result(
               #{ ?TYPE => ?COMMAND_ITEMNUM_OF_LIST
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                   , ?VALUE => <<"test list">>
                                   }
                                ,  constant_val(<<"zzzz">>)
                                ]
                }, Thread)
         of
             {ok, _, _} -> this_should_fail
         catch throw:Error:_StackTrace ->
                 ?assertMatch(#program_error{error=#list_not_set{list_name= <<"test list">>}},
                              Error),
                 ok
         end.

get_length_of_list_with_values() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [a, b, c]),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LENGTH_OF_LIST
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           ]
           }, Thread),
    ?assertMatch({ok, 3, _}, R).

get_length_of_list_not_defined() ->
    Thread = empty_thread(),
    R = automate_bot_engine_operations:get_result(
               #{ ?TYPE => ?COMMAND_LENGTH_OF_LIST
                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                   , ?VALUE => <<"test list">>
                                   }
                                ]
                }, Thread),
    ?assertMatch({ok, 0, _}, R).

list_contains_item_with_values_true() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [<<"a">>, <<"b">>, <<"c">>]),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_CONTAINS_ITEM
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           , constant_val(<<"b">>)
                           ]
           }, Thread),
    ?assertMatch({ok, true, _}, R).

list_contains_item_with_values_false() ->
    {ok, Thread} = automate_bot_engine_variables:set_program_variable(empty_thread(),  <<"test list">>, [<<"a">>, <<"b">>, <<"c">>]),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_CONTAINS_ITEM
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           , constant_val(<<"z">>)
                           ]
           }, Thread),
    ?assertMatch({ok, false, _}, R).

list_contains_item_not_defined() ->
    Thread = empty_thread(),
    R = automate_bot_engine_operations:get_result(
          #{ ?TYPE => ?COMMAND_LIST_CONTAINS_ITEM
           , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                              , ?VALUE => <<"test list">>
                              }
                           , constant_val(<<"z">>)
                           ]
           }, Thread),
    ?assertMatch({ok, false, _}, R).

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.

empty_thread() ->
    #program_thread{ position = [1]
                   , program=[undefined]
                   , global_memory=#{}
                   , instruction_memory=#{}
                   , program_id=binary:list_to_bin(uuid:to_string(uuid:uuid4()))
                   , thread_id=undefined
                   }.
