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
    case automate_storage:get_thread_from_id(ThreadId) of
        {ok, Thread} ->
            Pid = spawn_link(fun () -> init(ThreadId, Thread) end),
            {ok, Pid}
        %% We could guard agains this ↓. But as it shouldn't happen, better detect it ASAP
        %% {error, not_found} ->
        %%     ignore
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(binary(), #running_program_thread_entry{}) -> {ok, State :: term()} |
                                                      {ok, State :: term(), Timeout :: timeout()} |
                                                      {ok, State :: term(), hibernate} |
                                                      {stop, Reason :: term()} |
                                                      ignore.
init(ThreadId, Thread) ->
    ok = automate_storage:register_thread_runner(ThreadId, self()),

    self() ! {?SIGNAL_PROGRAM_TICK, {}},
    loop(#state{ thread=Thread
               , check_next_action=fun(_, _) -> continue end
               }).


-spec loop(#state{}) -> no_return().
loop(State = #state{ check_next_action = CheckContinue
                   , thread = Thread
                   }) ->
    receive
        {stop, From} ->
            From ! {?SERVER, ok},
            supervisor:terminate_child(automate_bot_engine_thread_runner_sup, self());
        {quit, _From} ->
            ok;
        {Signal, Message} ->
            NextState = case apply(CheckContinue, [State, {Signal, Message}]) of
                            continue ->
                                run_tick(State, {Signal, Message});
                            X ->
                                io:format("\033[47;30mIgnoring ~p (not applicable)\033[0m~n", [X]),
                                State
                        end,
            loop(NextState);
        {channel_engine, MonitorId, Message} ->
            %% Reemit so this will understand it
            self() ! {?TRIGGERED_BY_MONITOR, { MonitorId, Message }},
            loop(State);
        _Unknown ->
            %% io:fwrite("\033[47;30mIgnoring ~p\033[0m~n", [Unknown]),
            loop(State)
    end.

-spec run_tick(#state{}, any()) -> #state{}.
run_tick(State = #state{ thread=Thread }, Message) ->
    RunnerState = ?UTILS:parse_program_thread(Thread),
    NewRunnerState = case automate_bot_engine_operations:run_thread(RunnerState, Message) of
                          { stopped, _Reason } ->
                              self() ! {stop, self()},
                             RunnerState; %% Self-destroy
                          { did_not_run, {new_state, NewState} } ->
                              NewState;
                          { did_not_run, _Reason } ->
                              RunnerState;
                          { ran_this_tick, NewThreadState } ->
                              #running_program_thread_entry{ parent_program_id=Ppid } = Thread,
                              automate_stats:log_observation(counter, automate_bot_engine_cycles, [Ppid]),
                              NewThreadState
                      end,

    {ok, ExpectedSignals} = automate_bot_engine_operations:get_expected_signals([NewRunnerState]),

    %% Trigger now the timer signal if needed
    case lists:member(?SIGNAL_PROGRAM_TICK, ExpectedSignals) of
        true ->
            timer:send_after(?MILLIS_PER_TICK, self(), {?SIGNAL_PROGRAM_TICK, {}});
        _ ->
            ok
    end,
    #state{ thread=?UTILS:merge_thread_structures(Thread, NewRunnerState)
          , check_next_action=?UTILS:build_check_next_action(ExpectedSignals)
          }.
