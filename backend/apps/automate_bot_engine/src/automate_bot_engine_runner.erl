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

-record(state, { program_id
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
    loop(#state{ program_id=ProgramId }).

-spec loop(#state{}) -> no_return().
loop(State) ->
    receive
        {update, From} ->
            io:format("Updating ~p~n", [State]),
            From ! {?SERVER, ok},
            ?SERVER:loop(State);
        X ->
            io:format("Discarding ~p~n", [X]),
            loop(State)
    end.
