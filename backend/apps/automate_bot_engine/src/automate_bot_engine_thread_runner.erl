%%%-------------------------------------------------------------------
%%% @author kenkeiras <kenkeiras@becho>
%%% @copyright (C) 2018, kenkeiras
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2018 by kenkeiras <kenkeiras@becho>
%%%-------------------------------------------------------------------
-module(automate_bot_engine_thread_runner).

%% API
-export([ loop/1
        , start_link/1
        , stop/1
        , stop_by_id/1
        ]).

-define(SERVER, ?MODULE).
-define(UTILS, automate_bot_engine_thread_utils).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

-record(state, { thread :: #running_program_thread_entry{}
               , check_next_action
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec stop(pid()) -> ok.
stop(Pid) ->
    Pid ! { stop, self() },
    receive
        {?SERVER, X} ->
            X
    end.

-spec stop_by_id(binary()) -> ok.
stop_by_id(ThreadId) ->
    case automate_storage:get_thread_from_id(ThreadId) of
        {ok, #running_program_thread_entry{runner_pid=Pid}} ->
            stop(Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary()) -> {ok, Pid :: pid()} |
                              {error, Error :: {already_started, pid()}} |
                              {error, Error :: term()} |
                              ignore.
start_link(ThreadId) ->
    Pid = spawn_link(fun () -> init(ThreadId) end),
    {ok, Pid}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(binary()) -> {ok, State :: term()} |
                        {ok, State :: term(), Timeout :: timeout()} |
                        {ok, State :: term(), hibernate} |
                        {stop, Reason :: term()} |
                        ignore.
init(ThreadId) ->
    case automate_storage:register_thread_runner(ThreadId, self()) of
        {ok, NewThread} ->
            self() ! {?SIGNAL_PROGRAM_TICK, {}},
            loop(#state{ thread=NewThread
                       , check_next_action=fun(_, _) -> continue end
                       })
            %% We could guard agains this ↓. But as it shouldn't happen, better detect it ASAP
            %% {error, not_found} ->
            %%     ignore
    end.


-spec loop(#state{}) -> no_return().
loop(State = #state{ check_next_action = CheckContinue
                   , thread = Thread
                   }) ->
    receive
        {stop, From} ->
            From ! {?SERVER, ok},
            #running_program_thread_entry{ thread_id=ThreadId } = Thread,
            automate_storage:delete_thread(ThreadId),
            supervisor:terminate_child(automate_bot_engine_thread_runner_sup, self());
        {quit, _From} ->
            ok;
        {Signal, Message} ->
            NextState = case apply(CheckContinue, [State, {Signal, Message}]) of
                            continue ->
                                run_tick(State, {Signal, Message});
                            X ->
                                io:format("[~p:~p]\033[47;30mIgnoring ~p (not applicable)\033[0m~n", [?MODULE, ?LINE, X]),
                                State
                        end,
            loop(NextState);
        {channel_engine, MonitorId, Message} ->
            %% Re-emit so this will understand it
            self() ! {?TRIGGERED_BY_MONITOR, { MonitorId, Message }},
            loop(State);
        Unknown ->
            io:fwrite("\033[47;30mIgnoring ~p\033[0m~n", [Unknown]),
            loop(State)
    end.

-spec run_tick(#state{}, any()) -> #state{}.
run_tick(State = #state{ thread=Thread }, Message) ->
    #running_program_thread_entry{ thread_id=ThreadId } = Thread,
    RunnerState = ?UTILS:parse_program_thread(Thread),
    {UpdateThread, NewRunnerState} = case automate_bot_engine_operations:run_thread(RunnerState, Message, ThreadId) of
                                         { stopped, _Reason } ->
                                             self() ! {stop, self()},
                                             {false, RunnerState}; %% Self-destroy
                                         { did_not_run, {new_state, NewState} } ->
                                             {true, NewState};
                                         { did_not_run, _Reason } ->
                                             {false, RunnerState};
                                         { ran_this_tick, RanThreadState } ->
                                             #running_program_thread_entry{ parent_program_id=Ppid } = Thread,
                                             automate_stats:log_observation(counter, automate_bot_engine_cycles, [Ppid]),
                                             {true, RanThreadState}
                                     end,

    ExpectedSignals = case automate_bot_engine_operations:get_expected_signals([NewRunnerState]) of
                          {ok, []} ->
                              [?SIGNAL_PROGRAM_TICK];
                          {ok, Signals} ->
                              Signals
                      end,

    %% Trigger now the timer signal if needed
    case lists:member(?SIGNAL_PROGRAM_TICK, ExpectedSignals) of
        true ->
            erlang:send_after(?MILLIS_PER_TICK, self(), {?SIGNAL_PROGRAM_TICK, {}});
        _ ->
            ok
    end,

    NewThreadState = ?UTILS:merge_thread_structures(Thread, NewRunnerState),

    case UpdateThread of
        true ->
            automate_storage:update_thread(NewThreadState);
        false ->
            ok
    end,

    #state{ thread=NewThreadState
          , check_next_action=?UTILS:build_check_next_action(ExpectedSignals)
          }.
