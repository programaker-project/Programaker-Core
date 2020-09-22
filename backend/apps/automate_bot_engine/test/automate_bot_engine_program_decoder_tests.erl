%%% @doc
%%% Automate bot engine program decoder tests.
%%% @end

-module(automate_bot_engine_program_decoder_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").
-include("../src/program_records.hrl").
-include("../src/instructions.hrl").
-include("../../automate_channel_engine/src/records.hrl").

-define(APPLICATION, automate_bot_engine).
-define(TEST_NODES, [node()]).
-define(TEST_MONITOR, <<"__test_monitor__">>).
-define(TEST_SERVICE, automate_service_registry_test_service:get_uuid()).
-define(TEST_SERVICE_ACTION, test_action).

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
    NodeName = node(),

    %% %% Use a custom node name to avoid overwriting the actual databases
    %% net_kernel:start([testing, shortnames]),

    %% {ok, Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% application:stop(?APPLICATION),

    ok.


tests(_SetupResult) ->
    [ {"[Bot engine][Program decoder] Don't crash on undefined program", fun undefined_program_dont_crash/0}
    ].


undefined_program_dont_crash() ->
    ProgramId = <<"9a3f3d55-0393-4d0b-bfe8-08a7715230f8">>,
    R = automate_bot_engine_program_decoder:initialize_program(ProgramId,
                                                               #user_program_entry
                                                               { owner=undefined
                                                               , program_parsed=undefined
                                                               , enabled=true
                                                               }),

    ?assertMatch({ ok, #program_state{ program_id=ProgramId
                                     , variables=[]
                                     , triggers=[]
                                     , enabled=true
                                     }},
                 R).
