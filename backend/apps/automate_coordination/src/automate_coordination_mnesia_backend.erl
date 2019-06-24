%%%-------------------------------------------------------------------
%% @doc automate_coordination_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_coordination_mnesia_backend).

-export([ start_link/0
        , run_on_process_if_not_started/2
        , run_on_process_if_not_started_or_pid/3
        ]).

-define(RUN_ONCE_TASKS_TABLE, automate_coordination_run_once_tasks).

-record(run_once_tasks_table_entry, { task_id :: any()
                                    , node :: node()
                                    , pid :: pid()
                                    }).


%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),
    %% Live run once tasks
    ok = case mnesia:create_table(?RUN_ONCE_TASKS_TABLE,
                                  [ { attributes, record_info(fields, run_once_tasks_table_entry)}
                                  , { ram_copies, Nodes }
                                  , { record_name, run_once_tasks_table_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = mnesia:wait_for_tables([ ?RUN_ONCE_TASKS_TABLE
                                ], automate_configuration:get_table_wait_time()),
    ignore.


run_on_process_if_not_started(Id, CandidatePid) ->
    run_on_process_if_not_started_or_pid(Id, CandidatePid, undefined).

run_on_process_if_not_started_or_pid(Id, CandidatePid, DisqualifiedPid) ->
    Node = node(),
    Transaction = fun() ->
                          case mnesia:read(?RUN_ONCE_TASKS_TABLE, Id) of
                              [] ->
                                  ok = mnesia:write(?RUN_ONCE_TASKS_TABLE,
                                                    #run_once_tasks_table_entry{ task_id=Id
                                                                               , node=Node
                                                                               , pid=CandidatePid
                                                                               },
                                                    write),
                                  {ok, not_run_used_pid};
                              [#run_once_tasks_table_entry{task_id=DisqualifiedPid}] ->
                                  ok = mnesia:write(?RUN_ONCE_TASKS_TABLE,
                                                    #run_once_tasks_table_entry{ task_id=Id
                                                                               , node=Node
                                                                               , pid=CandidatePid
                                                                               },
                                                    write),
                                  {ok, not_run_used_pid};
                              [#run_once_tasks_table_entry{ task_id=_
                                                          , node=_OtherNode
                                                          , pid=CurrentPid
                                                          }] ->
                                  {ok, is_running, CurrentPid}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {error, Reason} ->
            {error, Reason}
    end.
