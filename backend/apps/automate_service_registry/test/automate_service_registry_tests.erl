%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_service_registry_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, automate_service_registry).
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
    net_kernel:start([?MODULE, shortnames]),

    {ok, Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName, Pid}.

%% @doc App infrastructure teardown.
%% @end
stop({NodeName, _Pid}) ->
    application:stop(?APPLICATION),

    %% %% Restore the original node name
    net_kernel:start([NodeName, shortnames]),
    ok.

tests(_SetupResult) ->
    [ {"[Service registry] Register a global service, query it", fun register_global_service/0}
    %% , {"[Permissioned registry] Register a service for a user, query it", fun register_service_to_user/0}
    %% , {"[Permissioned registry] Register a service for a user, query it with other user", fun register_service_to_user_not_showing/0}
    ].


%%%% Service registration
%% Register a service available for everyone
%% After that query it to check that it's returned.
register_global_service() ->
    TestServiceName = automate_service_registry_test_service:get_name(),
    {ok, ServiceId} = automate_service_registry:register(automate_service_registry_test_service),
    {ok, Services} = automate_service_registry:get_all_services(),
    ?assertMatch(#{ServiceId := #{ name := TestServiceName }}, Services).
