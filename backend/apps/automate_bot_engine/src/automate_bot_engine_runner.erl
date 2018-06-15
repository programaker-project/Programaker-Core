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
        ]).

-define(SERVER, ?MODULE).
-define(MILLIS_PER_TICK, 100).
-define(TICK_SIGNAL, tick).
-include("../../automate_storage/src/records.hrl").

-record(subprogram_state, { subid
                            %% Note that erlang 1-indexes lists!
                            %% For this reason, position also starts on 1.
                          , position
                          , ast
                          }).
-record(program_state, { variables
                       , subprograms
                       }).
-record(state, { program_id
               , program
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
    {ok, ProgramState} = initialize_program(Program),
    self() ! ?TICK_SIGNAL,
    loop(#state{ program_id=ProgramId
               , program=ProgramState
               , check_next_action=fun(_, _) -> continue end
               }).

-spec loop(#state{}) -> no_return().
loop(State = #state{check_next_action = CheckContinue}) ->
    receive
        {update, From} ->
            From ! {?SERVER, ok},
            ?SERVER:loop(State);
        {quit, _From} ->
            ok;
        X ->
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
    {ok, {NonRunnedPrograms, RunnedPrograms, ExpectedMessages}} = run_instructions(State, Message),
    State#state{ program=Program#program_state{ subprograms=NonRunnedPrograms ++ RunnedPrograms }
               , check_next_action=build_check_next_action(ExpectedMessages)
               }.

build_check_next_action(ExpectedMessages) ->
    fun(_, Message) ->
            io:format("Received ~p expecting ~p~n", [Message, ExpectedMessages]),
            case lists:member(Message, ExpectedMessages) of
                true ->
                    continue;
                _ ->
                    skip
            end
    end.

run_instructions(#state{ program=#program_state{ subprograms=Subprograms} }, Message) ->
    {NonRunnedPrograms, RunnedPrograms } = run_all_subprograms(Subprograms, Message),
    ExpectedMessages = get_expected_messages(NonRunnedPrograms ++ RunnedPrograms),

    %% Trigger now the timer signal if needed
    case lists:member(?TICK_SIGNAL, ExpectedMessages) of
        true ->
            timer:send_after(?MILLIS_PER_TICK, self(), ?TICK_SIGNAL);
        _ ->
            ok
    end,
    {ok, {NonRunnedPrograms, RunnedPrograms, ExpectedMessages}}.

run_all_subprograms(Subprograms, Message) ->
    run_all_subprograms(Subprograms, Message, [], []).

run_all_subprograms([], _, AccNonRunned, AccRunned) ->
    {AccNonRunned, AccRunned};
run_all_subprograms([Subprogram | T], Message, AccNonRunned, AccRunned) ->
    case run_subprogram(Subprogram, Message) of
        { did_run, NextSubprogramState } ->
            run_all_subprograms(T, Message, AccNonRunned, [NextSubprogramState | AccRunned]);
        { did_not_run, NextSubprogramState } ->
            run_all_subprograms(T, Message, [NextSubprogramState | AccNonRunned], AccRunned)
    end.

run_subprogram(Subprogram, Message) ->
    {ok, Instruction} = get_instruction(Subprogram),
    run_instruction(Instruction, Message, Subprogram).

run_instruction( #{ <<"type">> := <<"chat_whenreceivecommand">> }
               , telegram_received_message
               , Subprogram) ->
    io:format("We got it!!!!!~n", []),
    {did_run, increment_position(Subprogram)};

run_instruction( #{ <<"type">> := <<"chat_whenreceivecommand">> }
               , _
               , Subprogram) ->
    io:format("Waiting for ~p...~n", [telegram_received_message]),
    {did_not_run, Subprogram};


run_instruction( #{ <<"type">> := Type }
               , ?TICK_SIGNAL
               , Subprogram) ->
    io:format("Running along on ~p~n", [Type]),
    {did_run, increment_position(Subprogram)}.


get_expected_messages(Programs) ->
    AllExpectedMessages = get_all_expected_messages(Programs, []),
    AllExpectedMessages.

get_all_expected_messages([], Acc) ->
    Acc;
get_all_expected_messages([Subprogram=#subprogram_state{} | T], Acc) ->
    get_all_expected_messages(T, [get_expected_messages_for_subprogram(Subprogram) | Acc]).

get_expected_messages_for_subprogram(Subprogram) ->
    {ok, Instruction} = get_instruction(Subprogram),
    get_expected_messages_for_instruction(Instruction).

get_expected_messages_for_instruction(#{ <<"type">> := <<"chat_whenreceivecommand">> }) ->
    telegram_received_message;

get_expected_messages_for_instruction(Instruction) ->
    io:format("Instruction: ~p~n", [Instruction]),
    ?TICK_SIGNAL.

increment_position(Program = #subprogram_state{position=Position}) ->
    IncrementedInnermost = increment_innermost(Position),
    BackToParent = back_to_parent(Position),
    FollowInSameLevelState = Program#subprogram_state{position=IncrementedInnermost},
    BackToParentState = Program#subprogram_state{position=BackToParent},
    case get_instruction(FollowInSameLevelState) of
        {ok, _} ->
            FollowInSameLevelState;
        {error, element_not_found} ->
            BackToParentState
    end.

back_to_parent([]) ->
    [1];
back_to_parent(List) ->
    case lists:reverse(List) of
        [_] ->  %% When reached the end, restart
            [1];
        [_ | Tail] ->
            lists:reverse(Tail)
    end.

increment_innermost([]) ->
    [];
increment_innermost(List)->
    [Latest | Tail] = lists:reverse(List),
    lists:reverse([Latest + 1 | Tail]).

-spec get_instruction(#program_state{}) -> {ok, map()} | {error, element_not_found}.
get_instruction(#subprogram_state{ ast=_Ast, position=[]}) ->
    {error, not_initialized};

get_instruction(#subprogram_state{ ast=Ast, position=Position}) ->
    case resolve_block_with_position(Ast, Position) of
        {ok, Block} ->
            {ok, Block};
        {error, Reason} ->
            {error, Reason}
    end.

-spec resolve_block_with_position(list(), list()) -> {ok, map()}.
resolve_block_with_position(Ast, [Position | _]) when Position > length(Ast) ->
    {error, element_not_found};

resolve_block_with_position(Ast, [Position | T]) ->
    resolve_subblock_with_position(lists:nth(Position, Ast), T).

-spec resolve_subblock_with_position(list(), list()) -> {ok, map()}.
resolve_subblock_with_position(Element, []) ->
    {ok, Element};

resolve_subblock_with_position(#{<<"contents">> := Contents}, [Position | _]) when Position > length(Contents) ->
    {error, element_not_found};

resolve_subblock_with_position(#{<<"contents">> := Contents}, [Position | T]) ->
    resolve_subblock_with_position(lists:nth(Position, Contents), T).


-spec initialize_program(#user_program_entry{}) -> {ok, #program_state{}}.
initialize_program(#user_program_entry{
                      program_parsed=#{ <<"variables">> := Variables
                                      , <<"blocks">> := Blocks
                                      }}) ->
    { ok
    , #program_state{ variables=Variables
                    , subprograms=enumerate_map(
                                    fun(SubId, Block) ->
                                            #subprogram_state{ subid=SubId
                                                             , position=[1]
                                                             , ast=Block
                                                             }
                                    end, Blocks)
                    }}.

enumerate_map(F, List) ->
    enumerate_map(F, List, 1, []).

enumerate_map(_, [], _, Acc) ->
    lists:reverse(Acc);
enumerate_map(F, [H | T], Count, Acc) ->
    enumerate_map(F, T, Count + 1, [F(Count, H) | Acc]).

