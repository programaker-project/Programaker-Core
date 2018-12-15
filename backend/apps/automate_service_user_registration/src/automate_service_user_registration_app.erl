%%%-------------------------------------------------------------------
%% @doc automate_service_registry APP API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_user_registration_app).

-behaviour(application).

-define(APPLICATION, automate_service_user_registration).
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start(_StartType, _StartArgs) ->
    ?APPLICATION:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
