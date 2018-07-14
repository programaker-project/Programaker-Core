%%%-------------------------------------------------------------------
%% @doc automate_monitor_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_monitor_engine_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [ #{ id => automate_monitor_engine_runner_sup
                    , start => {automate_monitor_engine_runner_sup, start_link, []}
                    , restart => permanent
                    , shutdown => 2000
                    , type => supervisor
                    , modules => [automate_monitor_engine_runner_sup]
                    }],
    {ok, { {one_for_one, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
