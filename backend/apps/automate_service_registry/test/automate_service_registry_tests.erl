%%% @doc
%%% Automate channel engine tests.
%%% @end

-module(automate_service_registry_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPLICATION, automate_service_registry).
-define(TEST_NODES, [node()]).
-define(ALLOWED_USER, <<"allowed user">>).
-define(UNAUTHORISED_USER, <<"unauthorised user">>).

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

    {ok, Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName, Pid}.

%% @doc App infrastructure teardown.
%% @end
stop({NodeName, _Pid}) ->
    application:stop(?APPLICATION),

    ok.

tests(_SetupResult) ->
    [ { "[Service registry] Register a global service, query it"
      , fun register_global_service/0}
    , { "[Permissioned registry] Register a service for a user, query it"
      , fun register_service_to_user_no_user/0}
    , { "[Permissioned registry] Register a service for a user, query it with an associated user"
      , fun register_service_to_user_correct_user/0 }
    , { "[Permissioned registry] Register a service for a user, query it with other user"
      , fun register_service_to_user_unauthorised/0}
    , { "[Permissioned registry] Allow a user to use an already public service, query it"
      , fun register_service_doubly_authorised/0 }
    ].


%%%% Service registration
%% Register a service available for everyone
%% After that query it to check that it's returned.
register_global_service() ->
    TestServiceName = automate_service_registry_test_service:get_name(),
    {ok, ServiceId} = automate_service_registry:register_public(automate_service_registry_test_service),
    {ok, Services} = automate_service_registry:get_all_public_services(),
    ?assertMatch(#{ServiceId := #{ name := TestServiceName }}, Services).

%%%% Permissioned service registration
register_service_to_user_no_user() ->
    {ok, ServiceId} = automate_service_registry:register_private(automate_service_registry_test_service),
    {ok, Services} = automate_service_registry:get_all_public_services(),
    ?assertNotMatch(#{ServiceId := _}, Services).

register_service_to_user_correct_user() ->
    {ok, ServiceId} = automate_service_registry:register_private(automate_service_registry_test_service),
    ok = automate_service_registry:allow_user(ServiceId, ?ALLOWED_USER),
    {ok, Services} = automate_service_registry:get_all_services_for_user(?ALLOWED_USER),
    ?assertMatch(#{ServiceId := _}, Services).

register_service_to_user_unauthorised() ->
    {ok, ServiceId} = automate_service_registry:register_private(automate_service_registry_test_service),
    ok = automate_service_registry:allow_user(ServiceId, ?ALLOWED_USER),
    {ok, Services} = automate_service_registry:get_all_services_for_user(?UNAUTHORISED_USER),
    ?assertNotMatch(#{ServiceId := _}, Services).

%%%% Doubly authorised
register_service_doubly_authorised() ->
    {ok, ServiceId} = automate_service_registry:register_public(automate_service_registry_test_service),
    ok = automate_service_registry:allow_user(ServiceId, ?ALLOWED_USER),
    {ok, Services} = automate_service_registry:get_all_services_for_user(?ALLOWED_USER),
    ?assertMatch(#{ServiceId := _}, Services).
