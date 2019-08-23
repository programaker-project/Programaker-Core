%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_storage_tags_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, automate_storage).
%% Data structures
-include("../../automate_storage/src/records.hrl").

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
    NodeName = node(),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    {ok, Pid} = application:ensure_all_started(automate_storage),

    {NodeName, Pid}.

%% @doc App infrastructure teardown.
%% @end
stop({NodeName, _Pid}) ->
    application:stop(automate_storage),

    ok.

tests(_SetupResult) ->
    [ {"[Storage program tags] Get tags of an empty program", fun test_get_tags_empty_program/0},
      {"[Storage program tags] Get tags of an program created", fun test_get_tags_inserted/0}
    ].

get_uuid() ->
    <<"b2173c01-465c-4f6e-99df-ba2bfc608e94">>.

get_tags() ->
    [<<"red">>,<<"blue">>,<<"orange">>].

test_get_tags_empty_program() ->
    Uuid = get_uuid(),
    {ok, Tags} = automate_storage:get_tags_program_from_id(Uuid),
    ?assertMatch(Tags, []).
    %%ct:fail("This is a test").

test_get_tags_inserted() ->
    Tags = get_tags(),
    Uuid = get_uuid(),
    ok = automate_storage:register_program_tags(Uuid,Tags),
    {ok,Tags_stored} = automate_storage:get_tags_program_from_id(Uuid),
    ?assertMatch(Tags_stored,Tags).