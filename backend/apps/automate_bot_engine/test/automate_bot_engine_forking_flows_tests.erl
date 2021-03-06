%%% Automate bot engine getters tests.
%%% @end

-module(automate_bot_engine_forking_flows_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

%% Test data
-include("single_line_program.hrl").

-define(APPLICATION, automate_bot_engine).
-define(WAIT_PER_INSTRUCTION, 100).  %% Milliseconds
%% Note, if waiting per instruction takes too much time consider adding a method
%% which checks periodically.
-define(UTILS, automate_bot_engine_test_utils).

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

    {ok, _Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    %% Operations
    %% Lists
    [ {"[Bot engine][Fork Operations] Simple fork (no join)", fun simple_fork_no_join/0}
    , {"[Bot engine][Fork Operations] Simple fork with join", fun simple_fork_with_join/0}
    , {"[Bot engine][Fork Operations] Fork and join, check thread IDs", fun fork_and_join_check_thread_ids/0}
    , {"[Bot engine][Fork Operations] Nested fork and join, check thread IDs", fun nested_fork_and_join_check_thread_ids/0}
    , {"[Bot engine][Fork Operations] Fork fork and join first", fun fork_and_join_first/0}
    ].

%%%% Operations
simple_fork_no_join() ->
    ExpectedLogs = [<<"first branch">>, <<"second branch">>], %% Note that they might be shuffled

    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ {?COMMAND_FORK_EXECUTION, []
                                                         , [ [ { ?COMMAND_LOG_VALUE, [constant_val(<<"first branch">>)] } ]
                                                           , [ { ?COMMAND_LOG_VALUE, [constant_val(<<"second branch">>)] } ]
                                                           ]
                                                         }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 3),
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    [ #user_generated_log_entry{event_message=First}
    , #user_generated_log_entry{event_message=Second}
    ] = Logs,

    io:fwrite("Logs: ~p~n", [[First, Second]]),
    io:fwrite("Expected: ~p~n", [ExpectedLogs]),
    ?assert(([First, Second] =:= ExpectedLogs)
            or ([Second, First] =:= ExpectedLogs)).

simple_fork_with_join() ->
    ExpectedLogs = [<<"first branch">>, <<"second branch">>, <<"joined">>], %% Note that the first two might be shuffled

    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_FORK_EXECUTION, []
                                                         , [ [ {?COMMAND_LOG_VALUE, [constant_val(<<"first branch">>)]} ]
                                                           , [ {?COMMAND_LOG_VALUE, [constant_val(<<"second branch">>)]} ]
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [constant_val(<<"joined">>)] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 4),
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    [ #user_generated_log_entry{event_message=First}
    , #user_generated_log_entry{event_message=Second}
    , #user_generated_log_entry{event_message=Joined}
    ] = Logs,

    io:fwrite("Logs: ~p~n", [[First, Second]]),
    io:fwrite("Expected: ~p~n", [ExpectedLogs]),
    ?assert(([First, Second, Joined] =:= ExpectedLogs)
            or ([Second, First, Joined] =:= ExpectedLogs)).

fork_and_join_check_thread_ids() ->
    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                       , { ?COMMAND_FORK_EXECUTION, []
                                                         , [ [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                           , [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 5),
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    [ #user_generated_log_entry{event_message=Main}
    , #user_generated_log_entry{event_message=First}
    , #user_generated_log_entry{event_message=Second}
    , #user_generated_log_entry{event_message=Main2}
    ] = Logs,

    io:fwrite("Logs: ~p~n", [[Main, First, Second, Main2]]),
    ?assert((Main =:= Main2)
            and (First =/= Second)).

nested_fork_and_join_check_thread_ids() ->
    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                       , { ?COMMAND_FORK_EXECUTION, []
                                                         , [ [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                             , { ?COMMAND_FORK_EXECUTION, []
                                                               , [ [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                                 , [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                                 ]
                                                               }
                                                             , { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                             ]
                                                           , [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                             , { ?COMMAND_FORK_EXECUTION, []
                                                               , [ [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                                 , [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                                 ]
                                                               }
                                                             , { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                             ]
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 20),
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    Messages = [ Msg ||  #user_generated_log_entry{event_message=Msg} <- Logs ],
    io:fwrite("Logs: ~p~n", [Messages]),

    ?assertEqual(length(Messages), 10),

    %% The first and the last should be the same (before fork and after the merge).
    First = lists:nth(1, Messages),
    ?assertEqual(First, lists:nth(10, Messages)),

    %% The second should be repeated (as it's one of the top level forks, it will be joined).
    Second = lists:nth(2, Messages),
    ?assertEqual(length(lists:filter(fun (X) -> X =:= Second end, Messages)), 2),

    CountOcurrences = fun(E, L) ->
                              length(lists:filter(fun (Y) -> E =:= Y end, L))
                      end,

    %% Same for the one-to-last (in case is not the same as the Second)
    SecondFork = case lists:nth(9, Messages) of
                     Second -> %% If it's the same as Second, find the other duplicated one
                         {value, Result} = lists:search(fun(X) ->
                                                                case X of
                                                                    First -> false ;
                                                                    Second -> false;
                                                                    _ ->
                                                                        CountOcurrences(X, Messages) =:= 2
                                                                end
                                                        end, Messages),
                         Result;
                     _ ->
                         OneToLast = lists:nth(9, Messages),
                         ?assertEqual(length(lists:filter(fun (X) -> X =:= OneToLast end, Messages)), 2),
                         OneToLast
                 end,

    %% The rest should only appear once
    ?assert(lists:all(fun (X) ->
                              case X of
                                  First -> true;
                                  Second -> true;
                                  SecondFork -> true;
                                  _ ->
                                      %% No duplicates
                                      CountOcurrences(X, Messages) =:= 1
                              end
                      end, Messages)).

fork_and_join_first() ->
    ProgramId = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Thread = #program_thread{ position = [1]
                            , program=?UTILS:build_ast([ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                       , { ?COMMAND_FORK_EXECUTION, [ constant_val(?OP_FORK_CONTINUE_ON_FIRST) ]
                                                         , [ [ { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                           , [ { ?COMMAND_WAIT, [ constant_val(0.1) ] }
                                                             , { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] } ]
                                                           ]
                                                         }
                                                       , { ?COMMAND_LOG_VALUE, [ ?UTILS:block_val({ ?COMMAND_GET_THREAD_ID }) ] }
                                                       ])
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    {ok, _ThreadId} = automate_bot_engine_thread_launcher:launch_thread(ProgramId, Thread),
    timer:sleep(?WAIT_PER_INSTRUCTION * 5 + 200), %% Take into account the wait operation
    {ok, Logs} = automate_bot_engine:get_user_generated_logs(ProgramId),

    Messages = [ M || #user_generated_log_entry{event_message=M} <- Logs ],
    [ Main, First, Main2, Waited ] = Messages,

    io:fwrite("Logs: ~p~n", [[Main, First, Main2, Waited]]),
    ?assert((Main =:= Main2)
            and (First =/= Waited)).

%%====================================================================
%% Util functions
%%====================================================================
constant_val(Val) ->
    #{ ?TYPE => ?VARIABLE_CONSTANT
     , ?VALUE => Val
     }.
