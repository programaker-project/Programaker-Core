%%%-------------------------------------------------------------------
%% @doc automate_stats top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_stats_sup).

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
    {ok, { {one_for_one, 0, 1},
           [ ]
         } }.

%%====================================================================
%% Internal functions
%%====================================================================
