%%%-------------------------------------------------------------------
%% @doc automate_service_port_engine APP API
%% @end
%%%-------------------------------------------------------------------

%% @doc automate_service_port_engine APP API
-module(automate_service_port_engine_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    automate_stats:add_metric(counter,
                              automate_bridge_engine_messages_to_bridge,
                              <<"Automate bridge engine messages to bridge.">>,
                              [bridge_id]),
    automate_stats:add_metric(counter,
                              automate_bridge_engine_messages_from_bridge,
                              <<"Automate bridge engine messages from bridge.">>,
                              [bridge_id]),
    automate_service_port_engine_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
