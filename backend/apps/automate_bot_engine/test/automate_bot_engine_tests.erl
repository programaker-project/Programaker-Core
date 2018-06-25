%%% @doc
%%% Automate bot engine tests.
%%% @end

-module(automate_bot_engine_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").

%% Test data
-include("single_line_program.hrl").

-define(TEST_NODES, [node()]).

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

    %% %% Use a custom node name to avoid overwriting the actual databases
    %% net_kernel:start([?MODULE, shortnames]),
    %% hivemind_app:install(),
    %% {ok, Pid} = hivemind_app:start(),

    %% {NodeName, Pid}.
    ok.

%% @doc App infrastructure teardown.
%% @end
%% stop({NodeName, Pid}) ->
stop(ok) ->
    %% hivemind_app:stop(Pid),
    %% ok = mnesia:delete_schema(?TEST_NODES),

    %% %% Restore the original node name
    %% net_kernel:start([NodeName, shortnames]).
    ok.

tests(_SetupResult) ->
    [ {"[Bot runner][Initialization] Single line program initialization", fun single_line_program_initialization/0}
    ].


%%%% Bot runner
%% Initialization
single_line_program_initialization() ->
    Program  = ?SINGLE_LINE_PROGRAM,
    Expected = ?SINGLE_LINE_PROGRAM_INITIALIZATION,

    {ok, Expected} = automate_bot_engine_program_decoder:initialize_program(Program).
