-module(automate_storage_maintenance).

%% API exports
-export([ wait_table/1
        ]).

-include("./databases.hrl").
-define(FORCE_LOADING_SLEEP_TIME, 10000).
-define(MAX_LOAD_TABLE, 1000 * 60 * 60 * 4). %% 4 Hours

%%====================================================================
%% API functions
%%====================================================================
wait_table(Tab) ->
    Orig = self(),
    {_Pid, Ref} = spawn_monitor(fun() ->
                                        Orig ! {load_table, mnesia:wait_for_tables([Tab], ?MAX_LOAD_TABLE)}
                                end),
    wait_table_loading(Tab, Ref).

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
