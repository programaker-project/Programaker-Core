%%%-------------------------------------------------------------------
%% @doc automate engines supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_engines_sup).

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
    {ok, { { one_for_one, ?AUTOMATE_SUPERVISOR_INTENSITY, ?AUTOMATE_SUPERVISOR_PERIOD},
           [ #{ id => automate_channel_engine
              , start => { automate_channel_engine_sup, start_link, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_channel_engine]
              }
           , #{ id => automate_bot_engine
              , start => { automate_bot_engine_sup, start_link, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_bot_engine]
              }
           , #{ id => automate_service_registry
              , start => { automate_service_registry_sup, start_link, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_service_registry]
              }
           , #{ id => automate_monitor_engine
              , start => { automate_monitor_engine_sup, start_link, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_monitor_engine]
              }
           , #{ id => automate_service_port_engine
              , start => { automate_service_port_engine_sup, start_link, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_service_port_engine]
              }
           , #{ id => automate_template_engine
              , start => { automate_template_engine_app, start, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_template_engine]
              }
           , #{ id => automate_stats
              , start => { automate_stats_app, start, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_stats]
              }
           , #{ id => automate_services_time
              , start => { automate_services_time_app, start, [] }
              , restart => permanent
              , shutdown => 2000
              , type => supervisor
              , modules => [automate_services_time]
              }
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
