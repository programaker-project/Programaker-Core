%%%-------------------------------------------------------------------
%%% @author kenkeiras <kenkeiras@becho>
%%% @copyright (C) 2018, kenkeiras
%%% @doc
%%%
%%% @end
%%% Created : 13 Jun 2018 by kenkeiras <kenkeiras@becho>
%%%-------------------------------------------------------------------
-module(automate_bot_engine_runner).

%% API
-export([ update/1
        , loop/1
        , start_link/1
        , stop_program/1
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, { program
               , check_next_action
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec update(pid()) -> ok.
update(Pid) ->
    Pid ! { update, self() },
    receive
        {?SERVER, X} ->
            X
    end.

-spec stop_program(pid()) -> ok.
stop_program(Pid) ->
    Pid ! { stop_program, self() },
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
start_link(ProgramId) ->
    case automate_storage:get_program_from_id(ProgramId) of
        {ok, Program} ->
            case automate_coordination:run_task_not_parallel(fun() -> init(ProgramId, Program) end,
                                                             {?MODULE, ProgramId}) of
                {started, Pid} ->
                    true = link(Pid),
                    {ok, Pid};
                {already_running, Pid} ->
                    true = link(Pid),
                    {ok, Pid};
                {error, Error} ->
                    {error, Error}
            end;
        {error, not_found} ->
            ok = automate_storage:delete_running_process(ProgramId),
            ignore
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
-spec init(binary(), #user_program_entry{}) -> {ok, State :: term()} |
                                  {ok, State :: term(), Timeout :: timeout()} |
                                  {ok, State :: term(), hibernate} |
                                  {stop, Reason :: term()} |
                                  ignore.
init(ProgramId, Program) ->
    ok = automate_storage:register_program_runner(ProgramId, self()),
    {ok, ProgramState} = automate_bot_engine_program_decoder:initialize_program(ProgramId, Program),

    self() ! {?SIGNAL_PROGRAM_TICK, {}},
    loop(#state{ program=ProgramState
               , check_next_action=fun(_, _) -> continue end
               }).


-spec update_state(#program_state{}) -> #state{}.
update_state(State = #program_state{ program_id=ProgramId } ) ->
    {ok, Program} = automate_storage:get_program_from_id(ProgramId),
    {ok, RestartedProgram} = automate_bot_engine_program_decoder:update_program(State, Program),

    self() ! {?SIGNAL_PROGRAM_TICK, {}},
    #state{ program=RestartedProgram
          , check_next_action=fun(_, _) -> continue end
          }.

-spec loop(#state{}) -> no_return().
loop(State = #state{ check_next_action = CheckContinue
                   , program = Program
                   }) ->
    receive
        {stop_program, From} ->
            From ! {?SERVER, ok},
            supervisor:terminate_child(automate_bot_engine_runner_sup, self());
        {update, From} ->
            From ! {?SERVER, ok},
            ?SERVER:loop(update_state(Program));
        {quit, _From} ->
            ok;
        {Signal, Message} ->
            NextState = case apply(CheckContinue, [State, {Signal, Message}]) of
                            continue ->
                                try
                                    run_tick(State, {Signal, Message})
                                of
                                    NewState -> NewState
                                catch ErrNS:Err:StackTrace ->
                                        %% TODO: In this case we probably can stop the program
                                        %% as the triggers fail to work
                                        io:fwrite("Error running program tick ~p~n", [{ErrNS, Err, StackTrace}]),
                                        State
                                end;
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
            loop(State)
    end.

-spec run_tick(#state{}, any()) -> #state{}.
run_tick(State = #state{ program=Program }, Message) ->
    #program_state{program_id=Id} = Program,
    {ok, TriggeredThreads} = automate_bot_engine_triggers:get_triggered_threads(Program, Message),

    lists:foreach(fun(Thread) ->
                          automate_bot_engine_thread_launcher:launch_thread(Id, Thread)
                  end, TriggeredThreads),

    {ok, ExpectedSignals} = automate_bot_engine_triggers:get_expected_signals(Program),
    State#state{ program=Program
               , check_next_action=automate_bot_engine_thread_utils:build_check_next_action(ExpectedSignals)
               }.
