%%%-------------------------------------------------------------------
%% @doc automate_coordination top level API
%% @end
%%%-------------------------------------------------------------------

-module(automate_coordination).

-behaviour(application).

%% Module API
-export([run_task_not_parallel/2]).

%% Application callbacks
-export([start_link/0]).
-define(BACKEND, automate_coordination_mnesia_backend).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    ?BACKEND:start_link().

-spec run_task_not_parallel(function(), any()) -> {started, pid()}
                                                      | {already_running, pid()}
                                                      | {error, could_not_start}.
run_task_not_parallel(Function, Id) ->
    Parent = self(),
    %% Process prepared for continuation
    RunnerPid = spawn(fun() -> waiter(Parent, Function) end),

    Result = case ?BACKEND:run_on_process_if_not_started(Id, RunnerPid) of
                 {ok, not_run_used_pid} ->
                     RunnerPid ! continue,
                     {ok, RunnerPid};
                 {ok, is_running, Pid} ->
                     case process_info(Pid) of
                         undefined -> %% Stopped
                             case ?BACKEND:run_process_if_not_started_or_pid(Id, RunnerPid, Pid) of
                                 {ok, not_run_used_pid} ->
                                     RunnerPid ! continue,
                                     {ok, RunnerPid};
                                 {ok, is_running, Pid2} ->
                                     case process_info(Pid2) of
                                         undefined ->
                                             {error, could_not_start};
                                         _ ->
                                             RunnerPid ! cancel,
                                             {ok, Pid2}
                                     end
                             end;
                         _ ->
                             RunnerPid ! cancel,
                             {ok, Pid}
                     end
             end,
    case Result of
        {ok, RunnerPid} ->
            {started, RunnerPid};
        {ok, NewPid} ->
            {already_running, NewPid};
        X ->
            X
    end.

%%====================================================================
%% Internal functions
%%====================================================================
waiter(Parent, Function) ->
    receive
        continue ->
            Function();
        cancel ->
            ok;
        Msg ->
            io:fwrite("Unexpected message: ~p~n", [Msg]),
            waiter(Parent, Function)
    end.
