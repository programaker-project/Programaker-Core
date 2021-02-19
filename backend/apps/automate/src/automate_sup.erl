%%%-------------------------------------------------------------------
%% @doc automate top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_sup).

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
    automate_configuration_app:check_assertions(),
    {ok, { { rest_for_one, ?AUTOMATE_SUPERVISOR_INTENSITY, ?AUTOMATE_SUPERVISOR_PERIOD},
           [ #{ id => automate_configuration
              , start => { automate_configuration_distributed, start_link, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_configuration]
              }
           , #{ id => automate_logging
              , start => {automate_logging_app, start, []}
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_storage]
              }
           , #{ id => automate_storage
              , start => {automate_storage, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_storage]
              }
           , #{ id => automate_coordination
              , start => {automate_coordination, start_link, []}
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_coordination]
              }
           , #{ id => automate_engines
              , start => { automate_engines_app, start, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [ automate_engines ]
              }
           , #{ id => automate_rest_api
              , start => { automate_rest_api_app, start, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [ automate_services ]
              }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
