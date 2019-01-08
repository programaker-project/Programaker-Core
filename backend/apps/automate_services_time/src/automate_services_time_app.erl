%%%-------------------------------------------------------------------
%% @doc Timekeeping service starter
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_time_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = automate_service_registry:register_public(automate_services_time),
    automate_services_time:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
