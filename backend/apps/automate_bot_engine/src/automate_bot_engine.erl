%%%-------------------------------------------------------------------
%% @doc automate_bot_engine public
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine).

%% Application callbacks
-export([ stop_program_threads/2
        , change_program_status/3
        , get_user_from_pid/1
        , get_bridges_on_program/1
        , get_user_generated_logs/1
        ]).

-include("../../automate_storage/src/records.hrl").

-spec stop_program_threads(binary(),binary()) -> ok | {error, any()}.
stop_program_threads(_UserId, ProgramId) ->
    case automate_storage:get_threads_from_program(ProgramId) of
        { ok, Threads } ->
            lists:foreach(fun (ThreadId) ->
                                  automate_bot_engine_thread_runner:stop_by_id(ThreadId)
                          end, Threads),
            ok;
        { error, Reason } ->
            { error, Reason }
    end.

-spec change_program_status(binary(),binary(),boolean()) -> ok | {error, any()}.
change_program_status(Username, ProgramId, Status) ->
    case automate_storage:update_program_status(Username, ProgramId, Status) of
        ok ->
            ok = automate_bot_engine_launcher:update_program(ProgramId),
            ok;
        { error, Reason } ->
            { error, Reason }
    end.

-spec get_user_from_pid(pid()) -> { ok, owner_id() } | {error, not_found}.
get_user_from_pid(Pid) ->
    automate_storage:get_user_from_pid(Pid).

-spec get_bridges_on_program(#user_program_entry{}) -> { ok, [binary()] }.
get_bridges_on_program(Program) ->
    automate_bot_engine_program_decoder:get_bridges_on_program(Program).

-spec get_user_generated_logs(binary()) -> {error, not_found} | {ok, [#user_generated_log_entry{}]}.
get_user_generated_logs(Pid) ->
    automate_storage:get_user_generated_logs(Pid).
