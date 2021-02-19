%%%-------------------------------------------------------------------
%% @doc automate_rest_api public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_rest_api_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start() ->
    % Dependencies
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(cowboy),

    %% Initialize process
    automate_rest_api_sup:start_link().

start(_StartType, _StartArgs) ->
    start().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
