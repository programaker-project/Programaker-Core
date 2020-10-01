%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_program_linker_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, automate_program_linker).
-include("../../automate_bot_engine/src/instructions.hrl").

%%====================================================================
%% Test API
%%====================================================================

session_manager_test_() ->
    {setup
    , fun setup/0
    , fun stop/1
    , fun tests/1
    }.

%% @doc App infrastructure setup.
%% @end
setup() ->
    %% NodeName = node(),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    %% {ok, Pid} = application:ensure_all_started(?APPLICATION),
    {ok, _TimePid} = application:ensure_all_started(automate_services_time),

    %% {NodeName, Pid}.
    ok.

%% @doc App infrastructure teardown.
%% @end
stop(_) ->
    %% application:stop(?APPLICATION),

    ok.

tests(_SetupResult) ->
    [
    ].
