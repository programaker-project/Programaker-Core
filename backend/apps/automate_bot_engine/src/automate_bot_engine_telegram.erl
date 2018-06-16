%%%-------------------------------------------------------------------
%% @doc automate_bot_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_telegram).

%% API
-export([is_enabled/0]).

-define(APPLICATION, automate_bot_engine).

%%====================================================================
%% API functions
%%====================================================================

is_enabled() ->
    {ok, Enabled} = application:get_env(?APPLICATION, telegram_enabled),
    Enabled.
