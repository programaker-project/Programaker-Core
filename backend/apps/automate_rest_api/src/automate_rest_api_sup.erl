%%%-------------------------------------------------------------------
%% @doc automate_rest_api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_rest_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("../../automate_common_types/src/definitions.hrl").

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
    {ok, { {one_for_one, ?AUTOMATE_SUPERVISOR_INTENSITY, ?AUTOMATE_SUPERVISOR_PERIOD},
           [ #{ id => automate_rest_api_server
              , start => {automate_rest_api_server, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => worker
              , modules => [automate_rest_api_server]
              }

           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
