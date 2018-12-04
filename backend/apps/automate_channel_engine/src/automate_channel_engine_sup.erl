%%%-------------------------------------------------------------------
%% @doc automate_channel_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_sup).

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
           [ #{ id => automate_channel_engine_mnesia_backend
              , start => {automate_channel_engine_mnesia_backend, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [automate_channel_engine_mnesia_backend]
              }
           , #{ id => automate_channel_engine
              , start => {automate_channel_engine, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [automate_channel_engine]
              }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
