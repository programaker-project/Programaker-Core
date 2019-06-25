%%%-------------------------------------------------------------------
%% @doc automate_bot_engine main runner, allows to span appropiately
%%      the bots.
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_launcher).

%% API
-export([ update_program/1
        , stop_program/1
        ]).

%%====================================================================
%% API functions
%%====================================================================
update_program(ProgramId) ->
    case get_program_pid(ProgramId) of
        { ok, PID } ->
            io:fwrite("Process found on: ~p~n", [PID]),
            try process_info(PID) of
                undefined -> %% Not alive
                    automate_bot_engine_runner_sup:start(ProgramId);
                _ ->
                    automate_bot_engine_runner:update(PID)
            catch ErrorNs:Error ->
                    io:fwrite("[~p] When getting process info, raised: ~p~n", [?MODULE, {ErrorNs, Error}]),
                    automate_bot_engine_runner_sup:start(ProgramId)
            end;
        {error, not_running} ->
            automate_bot_engine_runner_sup:start(ProgramId)
    end.

-spec stop_program(binary()) -> {ok, already_stopped | stopped_now}.
stop_program(ProgramId) ->
    case get_program_pid(ProgramId) of
        { ok, PID } ->
            case process_info(PID) of
                undefined -> %% Not alive
                    ok = automate_storage:delete_running_process(ProgramId);
                _ ->
                    ok = automate_bot_engine_runner:stop_program(PID),
                    ok = automate_storage:delete_running_process(ProgramId)
            end,
            {ok, stopped_now};
        {error, not_running} ->
            {ok, already_stopped}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
get_program_pid(ProgramId) ->
    automate_storage:get_program_pid(ProgramId).
