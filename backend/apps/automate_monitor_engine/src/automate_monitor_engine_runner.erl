-module(automate_monitor_engine_runner).

%% API
-export([ update/1
        , loop/1
        , start_link/1
        ]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL, 1000 * 60 * 10). %% 10 minutes in milliseconds
-define(END_OF_INTERVAL_MESSAGE, {tick}).
-define(FIRST_CHECK_INTERVAL, random:uniform(1000 * 60)). %% Uniformly spread during the first minute
-include("../../automate_storage/src/records.hrl").

-record(state, { monitor
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
start_link(MonitorId) ->
    Pid = spawn_link(fun () -> init(MonitorId) end),
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
init(MonitorId) ->
    io:format("Starting ~p~n", [MonitorId]),
    Monitor = automate_storage:get_monitor_from_id(MonitorId),
    timer:send_after(?FIRST_CHECK_INTERVAL, ?END_OF_INTERVAL_MESSAGE),

    loop(#state{ monitor=Monitor
               }).

-spec loop(#state{}) -> no_return().
loop(State=#state{ monitor=Monitor
                 }) ->
    receive
        {update, From} ->
            From ! {?SERVER, ok},
            %% TODO: Update monitor content
            ?SERVER:loop(State);
        {quit, _From} ->
            ok;
        ?END_OF_INTERVAL_MESSAGE ->
            NextState = run(Monitor),
            timer:send_after(?CHECK_INTERVAL, ?END_OF_INTERVAL_MESSAGE),

            loop(State#state{ monitor=NextState })
    end.

run(Monitor) ->
    io:format("Triggering monitor: ~p~n", [Monitor]),
    Monitor.
