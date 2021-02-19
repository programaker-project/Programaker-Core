%%%-------------------------------------------------------------------
%% @doc automate_stats app API
%% @end
%%%-------------------------------------------------------------------

-module(automate_configuration_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, check_assertions/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    %% Check that configuration assertions are valid
    check_assertions(),
    automate_configuration_sup:start_link().


start(_StartType, _StartArgs) ->
    start().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
check_assertions() ->
    automate_configuration:get_program_logs_watermarks().
