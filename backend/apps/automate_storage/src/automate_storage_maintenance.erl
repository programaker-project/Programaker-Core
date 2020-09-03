-module(automate_storage_maintenance).

%% API exports
-export([ wait_table/1
        , prune_user_program_logs/0
        , get_db_status/0
        , flush_disc_table/1
        , with_backup/1
        ]).

-include("./databases.hrl").
-include("./records.hrl").

-define(FORCE_LOADING_SLEEP_TIME, 10000).
-define(MAX_LOAD_TABLE, 1000 * 60 * 60 * 6). %% 6 Hours
-define(MAX_PROGRAM_LOGS, 1000).

%%====================================================================
%% API functions
%%====================================================================
prune_user_program_logs() ->
    ok = wait_table(?USER_PROGRAM_LOGS_TABLE),
    {atomic, ok} = mnesia:transaction(fun() ->
                               ok = mnesia:write_lock_table(?USER_PROGRAM_LOGS_TABLE),
                               Keys = mnesia:all_keys(?USER_PROGRAM_LOGS_TABLE),
                               lists:foreach(fun(K) ->
                                                     Elements = mnesia:read(?USER_PROGRAM_LOGS_TABLE, K),
                                                     case length(Elements) > ?MAX_PROGRAM_LOGS of
                                                         true ->
                                                             Sorted = lists:sort(fun( #user_program_log_entry{ event_time=Time1 }
                                                                                    , #user_program_log_entry{ event_time=Time2 }
                                                                                    ) ->
                                                                                         Time1 >= Time2
                                                                                 end, Elements),
                                                             {Kept, _} = lists:split(?MAX_PROGRAM_LOGS + 1, Sorted),

                                                             %% Delete old values
                                                             mnesia:delete(?USER_PROGRAM_LOGS_TABLE, K, write),

                                                             %% Write new values
                                                             lists:foreach(fun(Element) ->
                                                                                   ok = mnesia:write(?USER_PROGRAM_LOGS_TABLE, Element, write)
                                                                           end, Kept);

                                                         %% If the limit of logs was not exceeded, do not make any change
                                                         _ -> ok
                                                     end
                                             end, Keys)
                       end),
    flush_disc_table(?USER_PROGRAM_LOGS_TABLE).

get_db_status() ->
    Tables = mnesia:system_info(tables),
    WordSize = erlang:system_info(wordsize),

    TableInfo = lists:map(fun(Tab) ->
                                  Size = mnesia:table_info(Tab, size),
                                  Ready = length(mnesia:table_info(Tab, active_replicas)) > 0,
                                  Memory = mnesia:table_info(Tab, memory) * WordSize,

                                  {Tab, [ { ready, Ready }
                                        , { size, Size }
                                        , { memory_kb, Memory / 1024 }
                                        ]}
                          end, Tables),
    NonReadyTables = lists:filtermap(fun({Tab, TabInfo}) ->
                                             case proplists:get_value(ready, TabInfo) of
                                                 true -> false;
                                                 false -> { true, Tab }
                                             end
                                     end, TableInfo),
    [{ tables, TableInfo }, {non_ready, NonReadyTables}].

wait_table(Tab) ->
    Orig = self(),
    {_Pid, Ref} = spawn_monitor(fun() ->
                                        Orig ! {load_table, mnesia:wait_for_tables([Tab], ?MAX_LOAD_TABLE)}
                                end),
    wait_table_loading(Tab, Ref).

flush_disc_table(Tab) ->
    with_backup(fun() ->
                        ok = lists:foreach(fun(Node) ->
                                                   io:fwrite("Flushing node: ~p...", [Node]),
                                                   mnesia:change_table_copy_type(Tab, Node, ram_copies),
                                                   mnesia:change_table_copy_type(Tab, Node, disc_copies),
                                                   io:fwrite(" ok!~n")
                                           end, mnesia:table_info(Tab, active_replicas))
                end).

with_backup(Fun) ->
    BackupName = "mnesia_" ++ integer_to_list(erlang:phash2(make_ref())),
    BackupDir = filename:basedir(user_cache, "automate"),
    BackupPath = BackupDir ++ "/" ++ BackupName,
    io:fwrite("Backing up to ~p~n", [BackupPath]),

    ok = filelib:ensure_dir(BackupPath), %% Yes, this is applied to the full path

    ok = mnesia:backup(BackupPath),
    try Fun() of
        X ->
            X
    catch ErrorNS:Error:StackTrace ->
            io:fwrite("~nError found, restoring..."),
            {atomic, _} = mnesia:restore(BackupPath, []),
            io:fwrite(" ok!~n"),

            %% Ideally we would re-raise/re-throw the exception here with no changes.
            %% I didn't yet find a way to do that
            throw({ErrorNS, Error, StackTrace})
    after
        ok = file:delete(BackupPath)
    end.


%%====================================================================
%% Internal functions
%%====================================================================
wait_table_loading(Tab, MonRef) ->
    receive { load_table, Result } ->
            %% After the `force_load` result, capture the close of the loader
            receive {'DOWN', MonRef, _Type, _Pid, _Reason} ->
                    Result
            end;

        %% Exited without answering. This most likely indicates an error
        {'DOWN', MonRef, _, _, Reason} ->
            {error, Reason}

    after ?FORCE_LOADING_SLEEP_TIME ->
            case mnesia:table_info(Tab, size) of
                {aborted, _} ->
                    ok;
                Size ->
                    io:fwrite("[~p] Loading... (current: ~p)~n", [Tab, Size])
            end,
            wait_table_loading(Tab, MonRef)
    end.
