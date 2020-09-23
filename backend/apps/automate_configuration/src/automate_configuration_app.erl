%%%-------------------------------------------------------------------
%% @doc automate_stats app API
%% @end
%%%-------------------------------------------------------------------

-module(automate_configuration_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% Check that configuration assertions are valid
    check_assertions(),
    automate_configuration_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
check_assertions() ->
    automate_configuration:get_program_logs_watermarks().
