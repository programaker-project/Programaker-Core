%%% Automate bot engine limits tests.
%%% @end

-module(automate_bot_engine_limits_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("single_line_program.hrl").
-include("../../automate_common_types/src/limits.hrl").

-define(APPLICATION, automate_bot_engine).

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

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    {ok, Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][Limits] Limit in string concatenation", fun limit_string_concatenation/0}
    , {"[Bot engine][Limits] Limit in list building", fun limit_add_to_list/0}
    ].

%%%% Operations
limit_string_concatenation() ->
    #program_thread{ program_id=ProgramId } = Thread = empty_thread(),
    ok = automate_bot_engine_variables:set_program_variable(ProgramId, <<"test">>, <<"">>, undefined),

    %% Builda  string just one block off from tripping the limit
    Block = <<"01234567">>,
    Iters = ceil((?USER_PROGRAM_MAX_VAR_SIZE + 1) / size(Block)) - 1,
    Chunk = binary:list_to_bin(lists:foldl(fun(_, Acc) -> [ Block | Acc ] end, [],  lists:seq(1, Iters))),

    %% Considering that the limit is 1M, we should be able to add 256×2048byte strings
    ?assertException(throw, {program_error, {memory_item_size_exceeded, _, ?USER_PROGRAM_MAX_VAR_SIZE } , _ },
                     automate_bot_engine_operations:run_instruction(
                       #{ ?TYPE => ?COMMAND_SET_VARIABLE
                        , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_VARIABLE
                                           , ?VALUE => <<"test">>
                                           }
                                        ,  #{ ?TYPE => ?VARIABLE_BLOCK
                                            , ?VALUE => [ #{ ?TYPE => ?COMMAND_JOIN
                                                           , ?ARGUMENTS => [ constant_val(Chunk)
                                                                           , constant_val(Block)
                                                                           ]
                                                           }
                                                        ]
                                            }
                                        ]
                        }, Thread, {?SIGNAL_PROGRAM_TICK, test})).

limit_add_to_list() ->
    #program_thread{ program_id=ProgramId } = Thread = empty_thread(),
    ok = automate_bot_engine_variables:set_program_variable(ProgramId, <<"test">>, [], undefined),

    %% Considering that the limit is 1M, we should be able to add 64×8192byte strings
    Block = <<"0123456789ABCDEF">>,
    Chunk = binary:list_to_bin(lists:foldl(fun(_, Acc) -> [ Block | Acc ] end, [],  lists:seq(1, 512))),

    Thread2 = lists:foldl(fun(_, InnerThread) ->
                                  {ran_this_tick, Thread2, _} = automate_bot_engine_operations:run_instruction(
                                                               #{ ?TYPE => ?COMMAND_ADD_TO_LIST
                                                                , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                                                   , ?VALUE => <<"test">>
                                                                                   }
                                                                                ,  constant_val(Chunk)
                                                                                ]
                                                                }, InnerThread, {?SIGNAL_PROGRAM_TICK, test}),

                                  Thread2
                          end, Thread, lists:seq(1, 64)),

    %% But adding 64 + 1 more should not be possible
    ?assertException(throw, {program_error, {memory_item_size_exceeded, _, ?USER_PROGRAM_MAX_VAR_SIZE } , _ },
                     lists:foldl(fun(_, InnerThread) ->
                                         {ran_this_tick, Thread3, _} = automate_bot_engine_operations:run_instruction(
                                                                      #{ ?TYPE => ?COMMAND_ADD_TO_LIST
                                                                       , ?ARGUMENTS => [ #{ ?TYPE => ?VARIABLE_LIST
                                                                                          , ?VALUE => <<"test">>
                                                                                          }
                                                                                       ,  constant_val(Chunk)
                                                                                       ]
                                                                       }, InnerThread, {?SIGNAL_PROGRAM_TICK, test}),

                                         Thread3
                                 end, Thread2, lists:seq(1, 64 + 1))).

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.

empty_thread() ->
    {_, _, ProgramId} = automate_bot_engine_test_utils:create_anonymous_program(),
    #program_thread{ position = [1]
                   , program=[undefined]
                   , global_memory=#{}
                   , instruction_memory=#{}
                   , program_id=ProgramId
                   , thread_id=undefined
                   }.
