%%%-------------------------------------------------------------------
%% @doc automate_storage top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_storage_sup).

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
           [ #{ id => automate_storage
              , start => {automate_storage, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [automate_storage]
              }

           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
