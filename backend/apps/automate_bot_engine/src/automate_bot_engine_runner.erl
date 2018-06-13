%%%-------------------------------------------------------------------
%% @doc automate_bot_engine simple runner. Takes a task to be run and
%%      executes it forever.
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_runner).

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
    {ok, { {simple_one_for_one, 0, 1},
           [ #{ id => automate_bot_engine_runner
              , start => {automate_bot_engine_runner, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [automate_bot_engine_runner]
              }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
