%%%-------------------------------------------------------------------
%% @doc automate_bot_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("databases.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = mnesia:wait_for_tables(?BOT_REQUIRED_DBS, automate_configuration:get_table_wait_time()),
    case mnesia:wait_for_tables(?BOT_EXTRA_DBS, automate_configuration:get_table_wait_time()) of
        ok -> ok;
        Result ->
            automate_logging:log_platform(error, io_lib:format("Error waiting for extra bot_engine tables: ~p", [Result]))
    end,
    automate_bot_engine_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
