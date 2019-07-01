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
        { ok, Pid } ->
            case automate_coordination_utils:is_process_alive(Pid) of
                false -> %% Not alive
                    automate_bot_engine_runner_sup:start(ProgramId);
                true ->
                    automate_bot_engine_runner:update(Pid)
            end;
        {error, not_running} ->
            automate_bot_engine_runner_sup:start(ProgramId)
    end.

-spec stop_program(binary()) -> {ok, already_stopped | stopped_now}.
stop_program(ProgramId) ->
    case get_program_pid(ProgramId) of
        { ok, Pid } ->
            case automate_coordination_utils:is_process_alive(Pid) of
                false -> %% Not alive
                    ok = automate_storage:delete_running_process(ProgramId);
                true ->
                    ok = automate_bot_engine_runner:stop_program(Pid),
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
