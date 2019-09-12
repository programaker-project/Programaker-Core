%%%-------------------------------------------------------------------
%% @doc automate_bot_engine public
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine).

%% Application callbacks
-export([stop_program_threads/2]).

-spec stop_program_threads(binary(),binary()) -> ok | {error, any()}.
stop_program_threads(Username, ProgramId) ->
    case automate_storage:get_threads_from_program(ProgramId) of
        { ok, Threads } ->
            lists:foreach(fun (ThreadId) ->
                                  automate_bot_engine_thread_runner:stop_by_id(ThreadId)
                          end, Threads),
            ok;
        { error, Reason } ->
            { error, Reason }
    end.
