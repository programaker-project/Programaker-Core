%%%-------------------------------------------------------------------
%% @doc automate_stats app API
%% @end
%%%-------------------------------------------------------------------

-module(automate_configuration_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    net_kernel:connect_node('backend@plaza-backend-0.plaza-backend.plaza-production.svc.cluster.local'),
    net_kernel:connect_node('backend@plaza-backend-1.plaza-backend.plaza-production.svc.cluster.local'),
    net_kernel:connect_node('backend@plaza-backend-2.plaza-backend.plaza-production.svc.cluster.local'),
    {ok, self()}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
