%%% @doc
%%% Automate service port custom blocks management tests.
%%% @end

-module(automate_service_port_engine_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-include("../../automate_storage/src/records.hrl").

%% Test data
-define(APPLICATION, automate_service_port_engine).
-define(TEST_NODES, [node()]).
-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(TEST_ID_PREFIX, "automate_service_port_engine_custom_blocks_tests").
-define(RECEIVE_TIMEOUT, 100).
-define(UTILS, automate_service_port_engine_test_utils).

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

    {ok, _Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_NodeName}) ->
    %% ?BACKEND:uninstall(),
    ok = application:stop(?APPLICATION),

    ok.

tests(_SetupResult) ->
    %% Custom blocks
    [ { "[Service Port - Custom blocks] Owned private blocks appear"
      , fun owned_private_blocks_appear/0
      }
    , { "[Service Port - Custom blocks] Non-owned private blocks don't appear"
      , fun non_owned_private_blocks_dont_appear/0
      }
    , { "[Service Port - Custom blocks] Owned public blocks appear"
      , fun owned_public_blocks_appear/0
      }
    , { "[Service Port - Custom blocks] Non-admin cannot set public bridge (and private blocks don't appear)"
      , fun non_admin_user_cannot_set_public_bridge/0
      }
    , { "[Service Port - Custom blocks] Non-owned public blocks appear"
      , fun non_owned_public_blocks_appear/0
      }
    , { "[Service Port - Custom blocks] Deleting a bridge deletes its custom blocks"
      , fun owned_delete_bridge_blocks/0
      }
    , { "[Service Port - Custom blocks] Connect to private block from group"
      , fun non_owned_public_blocks_from_group/0
      }
      %% Notification routing
    , { "[Service port - Notifications] Route notifications targeted to owner"
      , fun route_notification_targeted_to_owner/0
      }
    , { "[Service port - Notifications] Route notifications targeted to owner on public"
      , fun route_notification_targeted_to_owner_on_public/0
      }
    , { "[Service port - Notifications] Route notifications targeted to non-owner on public"
      , fun route_notification_targeted_to_non_owner_on_public/0
      }
    , { "[Service port - Notifications] Route notifications targeted to non-owner on non-public (because of non-admin)"
      , fun route_notification_targeted_to_non_owner_on_shadowed_public/0
      }
    , { "[Service port - Notifications] Route notifications to all users on public"
      , fun route_notification_targeted_to_all_users_on_public/0
      }
      %% Configuration
    , { "[Service port - Configuration - Owner] A public bridge can be later set to private"
      , fun set_public_bridge_to_private_owner/0
      }
    , { "[Service port - Configuration - Owner] A private bridge can be later set to public"
      , fun set_private_bridge_to_public_owner/0
      }
    ].


%%====================================================================
%% Custom block tests
%%====================================================================
owned_private_blocks_appear() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-test-1-service-port">>,

    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, _} = ?UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(OwnerUserId),

    check_test_block(CustomBlock).


non_owned_private_blocks_dont_appear() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    {_, RequesterUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-test-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    case ?BACKEND:gen_pending_connection(ServicePortId, RequesterUserId) of
        {ok, ConnectionId} ->
            %% TODO This connection should *not* be possible
            ?BACKEND:establish_connection(ServicePortId, RequesterUserId, ConnectionId, undefined);
        _ ->
            ok
    end,

    {ok, Results} = ?APPLICATION:list_custom_blocks(RequesterUserId),
    ?assertEqual(#{}, Results).


owned_public_blocks_appear() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    ServicePortName = <<?TEST_ID_PREFIX, "-test-3-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, _} = ?UTILS:establish_connection(ServicePortId, OwnerUserId),

    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(OwnerUserId),

    check_test_block(CustomBlock).

non_admin_user_cannot_set_public_bridge() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    {_, RequesterUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-test-4-1-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true %% This should be ignored as the user is not an admin
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    ?assertEqual({error, not_authorized}, ?UTILS:establish_connection(ServicePortId, RequesterUserId)),

    {ok, Results} = ?APPLICATION:list_custom_blocks(RequesterUserId),
    ?assertEqual(#{}, Results).


non_owned_public_blocks_appear() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    {_, RequesterUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-test-4-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, _} = ?UTILS:establish_connection(ServicePortId, RequesterUserId),

    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(RequesterUserId),

    check_test_block(CustomBlock).

owned_delete_bridge_blocks() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-test-5-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),
    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, _} = ?UTILS:establish_connection(BridgeId, OwnerUserId),

    {ok, #{ BridgeId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(OwnerUserId),
    %% Blocks are created
    check_test_block(CustomBlock),

    {ok, ServiceId} = automate_service_port_engine_mnesia_backend:get_service_id_for_port(BridgeId),
    ?assertMatch({ok, _}, automate_service_registry:get_service_by_id(ServiceId)),

    {ok, ResultsOk} = ?APPLICATION:list_custom_blocks(OwnerUserId),
    ?assertMatch({ok, _}, maps:find(ServiceId, ResultsOk)),
    {ok, _BeforeDeleteServices} = automate_service_registry:get_all_services_for_user(OwnerUserId),

    %% Delete bridge
    ok = automate_service_port_engine:delete_bridge(OwnerUserId, BridgeId),

    %% Blocks are destroyed
    {ok, Results} = ?APPLICATION:list_custom_blocks(OwnerUserId),
    ?assertEqual(error, maps:find(ServiceId, Results)),

    %% Service deregistred
    ?assertEqual({error, not_found}, automate_service_port_engine_mnesia_backend:get_service_id_for_port(BridgeId)),
    ?assertEqual({error, not_found}, automate_service_registry:get_service_by_id(ServiceId)),

    {ok, Services} = automate_service_registry:get_all_services_for_user(OwnerUserId),
    ?assertEqual(error, maps:find(ServiceId, Services)).


non_owned_public_blocks_from_group() ->
    ServicePortName = <<?TEST_ID_PREFIX, "-test-4-3-service-port">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-4-3-group">>,

    {_, OwnerId={user, OwnerUserId}} = ?UTILS:create_random_user(),
    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUserId, false),
    ok = automate_storage:update_group_metadata(GroupId, #{ min_level_for_private_bridge_usage => admin }),

    {ok, ServicePortId} = ?APPLICATION:create_service_port({group, GroupId}, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, {group, GroupId},
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, _} = ?UTILS:establish_connection(ServicePortId, OwnerId),

    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(OwnerId),

    check_test_block(CustomBlock).


%%====================================================================
%% Custom block tests - Internal functions
%%====================================================================
-define(Arguments, []).
-define(FunctionName, <<"first_function_name">>).
-define(FunctionId, <<"first_function_id">>).
-define(FunctionMessage, <<"sample message">>).
-define(BlockType, <<"str">>).
-define(BlockResultType, null).
-define(SaveTo, undefined).

get_test_block() ->
    #{ <<"arguments">> => ?Arguments
     , <<"function_name">> => ?FunctionName
     , <<"message">> => ?FunctionMessage
     , <<"id">> => ?FunctionId
     , <<"block_type">> => ?BlockType
     , <<"block_result_type">> => ?BlockResultType
     }.

check_test_block(Block) ->
    ?assertMatch(#service_port_block{ block_id=?FunctionId
                                    , function_name=?FunctionName
                                    , message=?FunctionMessage
                                    , arguments=?Arguments
                                    , block_type=?BlockType
                                    , block_result_type=?BlockResultType
                                    , save_to=?SaveTo
                                    }, Block).
-undef(Arguments).
-undef(FunctionName).
-undef(FunctionId).
-undef(FunctionMessage).
-undef(BlockType).
-undef(BlockResultType).
-undef(SaveTo).

%%====================================================================
%% Notification routing tests
%%====================================================================
route_notification_targeted_to_owner() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    TargetUserId = OwnerUserId,
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-1-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Listen on the service port
    {ok, _} = ?UTILS:establish_connection(ServicePortId, TargetUserId),
    ok = automate_service_registry_query:listen_service(ServicePortId, TargetUserId, {undefined, undefined}),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              TargetUserId, #{ <<"test">> => 1 }),

    %% Catch notification
    receive {channel_engine, _ChannelId, ReceivedMessage} ->
            ?assertEqual(ExpectedContent, ReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.


route_notification_targeted_to_owner_on_public() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    TargetUserId = OwnerUserId,
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Listen on the service port
    {ok, _} = ?UTILS:establish_connection(ServicePortId, TargetUserId),
    ok = automate_service_registry_query:listen_service(ServicePortId, TargetUserId, {undefined, undefined}),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              TargetUserId, #{ <<"test">> => 2 }),

    %% Catch notification
    receive {channel_engine, _ChannelId, ReceivedMessage} ->
            ?assertEqual(ExpectedContent, ReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.

route_notification_targeted_to_non_owner_on_public() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    {_, TargetUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-3-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Listen on the service port
    {ok, _} = ?UTILS:establish_connection(ServicePortId, TargetUserId),
    ok = automate_service_registry_query:listen_service(ServicePortId, TargetUserId, {undefined, undefined}),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              TargetUserId, #{ <<"test">> => 3 }),

    %% Catch notification
    receive {channel_engine, _ChannelId, ReceivedMessage} ->
            ?assertEqual(ExpectedContent, ReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.

route_notification_targeted_to_non_owner_on_shadowed_public() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    %% User not set to admin
    {_, TargetUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-3-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true %% This configuration is set to `is_public => false` as the owner is not admin
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Listen on the service port
    ?assertEqual({error, not_authorized}, ?UTILS:establish_connection(ServicePortId, TargetUserId)).

route_notification_targeted_to_all_users_on_public() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    {_, TargetUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-4-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Listen on the service port for non-owner
    {ok, _} = ?UTILS:establish_connection(ServicePortId, TargetUserId),
    ok = automate_service_registry_query:listen_service(ServicePortId, TargetUserId, {undefined, undefined}),

    %% Listen on the service port for owner
    {ok, _} = ?UTILS:establish_connection(ServicePortId, OwnerUserId),
    ok = automate_service_registry_query:listen_service(ServicePortId, OwnerUserId, {undefined, undefined}),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              null, #{ <<"test">> => 4 }),

    %% Get notification twice
    receive {channel_engine, _NonOwnerChannelId, NonOwnerReceivedMessage} ->
            ?assertEqual(ExpectedContent, NonOwnerReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(notif_to_all_users_non_owner_not_received)
    end,
    receive {channel_engine, _OwnerChannelId, OwnerReceivedMessage} ->
            ?assertEqual(ExpectedContent, OwnerReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(notif_to_all_users_owner_not_received)
    end.


%%====================================================================
%% Configuration
%%====================================================================
set_public_bridge_to_private_owner() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    {_, RequesterUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-5-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Found Owner and externals
    ?assertMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(OwnerUserId)),
    ?assertMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(RequesterUserId)),

    %% Set to private
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => #{ <<"is_public">> => false
                                                           , <<"service_name">> => ServicePortName
                                                           , <<"blocks">> => [ ]
                                                           }
                                         }),
    %% Still found for owner
    ?assertMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(OwnerUserId)),

    %% Not found for externals
    {ok, Results} = ?APPLICATION:list_custom_blocks(RequesterUserId),
    ?assertEqual(#{}, Results).

set_private_bridge_to_public_owner() ->
    {_, OwnerUserId} = ?UTILS:create_random_user(),
    ok = ?UTILS:set_admin_user(OwnerUserId),
    {_, TargetUserId} = ?UTILS:create_random_user(),
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-7-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    %% Found for owner, but NOT for externals
    ?assertMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(OwnerUserId)),
    ?assertNotMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(TargetUserId)),

    %% Set to public
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => #{ <<"is_public">> => true
                                                           , <<"service_name">> => ServicePortName
                                                           , <<"blocks">> => [ ]
                                                           }
                                         }),
    %% Found on owner AND on externals
    ?assertMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(OwnerUserId)),
    ?assertMatch({ok, #{ ServicePortId := _ }}, automate_service_registry:get_all_services_for_user(TargetUserId)).


%%====================================================================
%% Notification routing tests - Internal functions
%%====================================================================
emit_notification(ServicePortId, OwnerUserId, TargetUserId, Content) ->
    Key = binary:list_to_bin(atom_to_list(?MODULE)),
    Value = Content, %% For simplicity

    ToUserId = case TargetUserId of
                   null -> null;
                   _ ->
                       {ok, ConnectionId} = ?APPLICATION:internal_user_id_to_connection_id(TargetUserId,
                                                                                           ServicePortId),
                       ConnectionId
               end,
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        #{ <<"type">> => <<"NOTIFICATION">>
                                         , <<"key">> => Key
                                         , <<"to_user">> => ToUserId
                                         , <<"value">> => Value
                                         , <<"content">> => Content
                                         }),
    {ok, #{ <<"content">> => Content
          , <<"key">> => Key
          , <<"value">> => Value
          , <<"subkey">> => undefined
          , <<"service_id">> => ServicePortId
          }}.
