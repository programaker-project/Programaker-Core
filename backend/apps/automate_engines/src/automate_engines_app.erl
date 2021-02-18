%%%-------------------------------------------------------------------
%% @doc automate engines initialization
%% @end
%%%-------------------------------------------------------------------

-module(automate_engines_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
start() ->
    %% Dependencies
    {ok, _} = application:ensure_all_started(prometheus),
    %% Start supervisor
    automate_engines_sup:start_link().


start(_StartType, _StartArgs) ->
    start().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
