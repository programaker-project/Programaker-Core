%%%-------------------------------------------------------------------
%% @doc automate_services_telegram APP API
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_telegram_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case automate_services_telegram:is_enabled() of
        true ->
            {ok, _} = automate_service_registry:register_public(automate_services_telegram),
            automate_services_telegram_sup:start_link();
        false ->
            ignore
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
