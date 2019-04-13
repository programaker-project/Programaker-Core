%%%-------------------------------------------------------------------
%% @doc automate_channel_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    automate_stats:add_metric(counter,
                              automate_channel_engine_messages_in,
                              <<"Automate channel engine messages received (input).">>,
                              [channel_id]),
    automate_stats:add_metric(counter,
                              automate_channel_engine_messages_out,
                              <<"Automate channel engine messages passed (ouput).">>,
                              [channel_id]),
    automate_channel_engine_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
