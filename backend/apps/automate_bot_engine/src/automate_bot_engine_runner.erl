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
        , user_sent_message/4
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

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

user_sent_message(Pid, ChatId, Content, BotName) ->
    io:format("[~p] Message: ~p~n", [Pid, {?SIGNAL_TELEGRAM_MESSAGE_RECEIVED, {ChatId, Content, BotName}}]),
    Pid ! {?SIGNAL_TELEGRAM_MESSAGE_RECEIVED, {ChatId, Content, BotName}}.

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
    Pid = spawn_link(fun () -> init(ProgramId) end),
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
-spec init(Args :: term()) -> {ok, State :: term()} |
                              {ok, State :: term(), Timeout :: timeout()} |
                              {ok, State :: term(), hibernate} |
                              {stop, Reason :: term()} |
                              ignore.
init(ProgramId) ->
    io:format("Starting ~p~n", [ProgramId]),
    automate_storage:register_program_runner(ProgramId, self()),
    Program = automate_storage:get_program_from_id(ProgramId),
    {ok, ProgramState} = automate_bot_engine_program_decoder:initialize_program(ProgramId, Program),

    self() ! {?SIGNAL_PROGRAM_TICK, {}},
    loop(#state{ program=ProgramState
               , check_next_action=fun(_, _) -> continue end
               }).

-spec update_state(#program_state{}) -> #state{}.
update_state(State = #program_state{ program_id=ProgramId } ) ->
    Program = automate_storage:get_program_from_id(ProgramId),
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
        {update, From} ->
            From ! {?SERVER, ok},
            ?SERVER:loop(update_state(Program));
        {quit, _From} ->
            ok;
        X = {_, _} ->
            NextState = case apply(CheckContinue, [State, X]) of
                            continue ->
                                run_tick(State, X);
                            _ ->
                                io:format("Ignoring ~p~n", [X]),
                                State
                        end,
            loop(NextState)
    end.

-spec run_tick(#state{}, any()) -> #state{}.
run_tick(State = #state{ program=Program }, Message) ->
    #program_state{threads=OriginalThreads} = Program,
    {ok, TriggeredThreads} = automate_bot_engine_triggers:get_triggered_threads(Program, Message),

    ThreadsBefore = TriggeredThreads ++ OriginalThreads,

    {ok, {NonRunnedPrograms, RunnedPrograms}} = automate_bot_engine_operations:run_threads(ThreadsBefore, State, Message),

    ThreadsAfter = NonRunnedPrograms ++ RunnedPrograms,

    {ok, TriggersExpectedSignals} = automate_bot_engine_triggers:get_expected_signals(Program),
    {ok, ThreadsExpectedSignals} = automate_bot_engine_operations:get_expected_signals(ThreadsAfter),
    ExpectedSignals = TriggersExpectedSignals ++ ThreadsExpectedSignals,

    %% Trigger now the timer signal if needed
    case lists:member(?SIGNAL_PROGRAM_TICK, ExpectedSignals) of
        true ->
            timer:send_after(?MILLIS_PER_TICK, self(), {?SIGNAL_PROGRAM_TICK, {}});
        _ ->
            ok
    end,

    State#state{ program=Program#program_state{ threads=ThreadsAfter }
               , check_next_action=build_check_next_action(ExpectedSignals)
               }.


build_check_next_action(ExpectedMessages) ->
    fun(_, {Type, _Content}) ->
            io:format("Received ~p expecting ~p :: ~p~n", [Type, ExpectedMessages, lists:member(Type, ExpectedMessages)]),
            case lists:member(Type, ExpectedMessages) of
                true ->
                    continue;
                _ ->
                    skip
            end
    end.
