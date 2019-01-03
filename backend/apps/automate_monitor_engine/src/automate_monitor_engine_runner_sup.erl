%%%-------------------------------------------------------------------
%% @doc automate_monitor_engine supervisor for each task being run
%%      (also the one that spawns them).
%% @end
%%%-------------------------------------------------------------------

-module(automate_monitor_engine_runner_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0
        , start/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start(MonitorId) ->
    supervisor:start_child(?SERVER, [MonitorId]),
    ok.

start_link() ->
    automate_stats:add_metric(counter, automate_monitor_trigger, "Automate monitor check.",
                              [monitor_id, monitor_name]),
    Result = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    ok = start_running_monitors(),
    Result.


%%====================================================================
%% Supervisor callbacks
%%====================================================================



%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {simple_one_for_one, 3, 60},
           [ #{ id => automate_monitor_engine_runner
              , start => {automate_monitor_engine_runner, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [automate_monitor_engine_runner]
              }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
start_running_monitors() ->
    {ok, Ids} = automate_storage:dirty_list_monitors(),
    lists:foreach(fun(MonitorId) ->
                          ok = start(MonitorId)
                  end, Ids),
    ok.
