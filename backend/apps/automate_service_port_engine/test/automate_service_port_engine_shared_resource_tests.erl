%%% @doc
%%% Automate service port shared resource management tests.
%%% @end

-module(automate_service_port_engine_shared_resource_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_bot_engine/src/instructions.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").


%% Test data
-define(TEST_NODES, [node()]).
-define(TEST_ID_PREFIX, "automate_service_port_engine_shared_resource_tests").
-define(RECEIVE_TIMEOUT, 100).

-define(APPLICATION, automate_service_port_engine).
-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(UTILS, automate_service_port_engine_test_utils).
-define(BOT_UTILS, automate_bot_engine_test_utils).
-define(ROUTER, automate_service_port_engine_router).

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
    [ { "[Bridge - Shared resources] Allow to share resources, blocks that require a shared resource do appear"
      , fun blocks_with_shared_resources_appear/0
      }
    , { "[Bridge - Shared resources] Allow to share resources, blocks that require a shared resource do appear (multiple resources)"
      , fun blocks_with_shared_resources_appear_multiple_resources/0
      }
    , { "[Bridge - Shared resources] Allow to share resources, blocks that require a non-shared resource don't appear"
      , fun non_shared_resources_negate_custom_blocks/0
      }
    , { "[Bridge - Shared resources] Allow to share resources, blocks that require a non-shared resource don't appear (multiple resources)"
      , fun non_shared_resources_negate_custom_blocks_multiple_resources/0
      }
    , { "[Bridge - Shared resources] Allow to share resources, blocks that don't require resources don't appear"
      , fun shared_block_with_no_resources_dont_appear/0
      }
      %% Execution test
    , { "[Bridge - Shared resources] Allow to make calls on shared resource values"
      , fun allow_to_make_calls_on_shared_resource_values/0
      }
    , { "[Bridge - Shared resources] Don't to make calls on non-shared resource values"
      , fun disallow_calls_on_non_shared_resource_values/0
      }
      %% TODO: For this to be supported consider the TO-DO on automate_service_port_engine_service:get_connection()
      %% , { "[Bridge - Shared resources] Allow to make calls on shared resource values (multiple resources)"
      %%   , fun allow_to_make_calls_on_shared_resource_values_multiple_resources/0
      %%   }
    , { "[Bridge - Shared resources] Don't to make calls on non-shared resource values (multiple resources)"
      , fun disallow_calls_on_non_shared_resource_values_multiple_resources/0
      }
      %% Routing tests
    , { "[Bridge - Shared resources] Allow to listen on shared resource values"
      , fun allow_to_listen_on_shared_resource_values/0
      }
    , { "[Bridge - Shared resources] Don't allow to listen on non-shared resource values (no subkey)"
      , fun disallow_to_listen_on_non_shared_resources/0
      }
    , { "[Bridge - Shared resources] Don't allow to listen on non-shared resource values (different subkey)"
      , fun disallow_to_listen_on_shared_resource_different_subkey/0
      }
    , { "[Bridge - Shared resources] Listening in a subkey doesn't make program receive different subkey messages"
      , fun listening_on_shared_does_not_receive_different_subkeys/0
      }
    , { "[Bridge - Shared resources] Listening in a subkey doesn't make program receive different null subkey messages"
      , fun listening_on_shared_does_not_receive_null_subkeys/0
      }
    ].


%%====================================================================
%% Custom block tests
%%====================================================================
blocks_with_shared_resources_appear() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-1-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-1-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-1-group">>,
    ResourceName = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-1-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    {ok, #{ BridgeId := [CustomBlock] } } = automate_service_port_engine:list_custom_blocks({group, GroupId}),
    check_test_block(CustomBlock).

blocks_with_shared_resources_appear_multiple_resources() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-11-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-11-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-11-group">>,
    ResourceName1 = <<"channels">>,
    SharedValue1 = <<"shared-val-id1">>,
    SharedValueName1 = <<"shared-val-name1">>,
    ResourceName2 = <<"group">>,
    SharedValue2 = <<"shared-val-id2">>,
    SharedValueName2 = <<"shared-val-name2">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-11-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName1}, {resource, ResourceName2}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName1
                                                         , #{ SharedValue1 =>
                                                                  #{ <<"name">> => SharedValueName1
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName2
                                                         , #{ SharedValue2 =>
                                                                  #{ <<"name">> => SharedValueName2
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    {ok, #{ BridgeId := [CustomBlock] } } = automate_service_port_engine:list_custom_blocks({group, GroupId}),
    check_test_block(CustomBlock).

non_shared_resources_negate_custom_blocks() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-2-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-2-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-2-group">>,
    ResourceNameShared = <<"channels">>,
    ResourceNameNotShared = <<"non-channels">>,

    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-2-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceNameNotShared}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceNameShared
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ?assertMatch({ok, #{ BridgeId := [] } }, automate_service_port_engine:list_custom_blocks({group, GroupId})).

non_shared_resources_negate_custom_blocks_multiple_resources() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-21-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-21-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-21-group">>,
    ResourceNameNotShared = <<"non-channels">>,

    ResourceNameShared1 = <<"channels1">>,
    SharedValue1 = <<"shared-val-id1">>,
    SharedValueName1 = <<"shared-val-name1">>,
    ResourceNameShared2 = <<"groups">>,
    SharedValue2 = <<"share-val-id2">>,
    SharedValueName2 = <<"shared-val-name2">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-21-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceNameShared1}, {resource, ResourceNameNotShared}])
                                       , get_test_block([{resource, ResourceNameNotShared}, {resource, ResourceNameShared1}])
                                       ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceNameShared1
                                                         , #{ SharedValue1 =>
                                                                  #{ <<"name">> => SharedValueName1
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceNameShared2
                                                         , #{ SharedValue2 =>
                                                                  #{ <<"name">> => SharedValueName2
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ?assertMatch({ok, #{ BridgeId := [] } }, automate_service_port_engine:list_custom_blocks({group, GroupId})).


shared_block_with_no_resources_dont_appear() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-22-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-22-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-22-group">>,
    ResourceNameShared = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-22-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceNameShared
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ?assertMatch({ok, #{ BridgeId := [] } }, automate_service_port_engine:list_custom_blocks({group, GroupId})).


allow_to_make_calls_on_shared_resource_values() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-3-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-3-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-3-group">>,
    ResourceName = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-3-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),

    BridgePid = test_bridge(BridgeId),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    {ok, ProgramId} =  ?BOT_UTILS:create_user_program({group, GroupId}),
    Thread = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_CALL_SERVICE
                                         , ?ARGUMENTS => #{ ?SERVICE_ID => BridgeId
                                                          , ?SERVICE_ACTION => get_function_id()
                                                          , ?SERVICE_CALL_VALUES => [#{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                      , ?VALUE => SharedValue
                                                                                      }]
                                                          }
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    ?assertMatch({ran_this_tick, NewThreadState, _}, automate_bot_engine_operations:run_thread(Thread, {?SIGNAL_PROGRAM_TICK, none}, undefined)).

disallow_calls_on_non_shared_resource_values() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-4-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-4-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-4-group">>,
    ResourceName = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    NonSharedValue = <<"non-shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-4-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),

    Bridge = test_bridge(BridgeId),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    {ok, ProgramId} =  ?BOT_UTILS:create_user_program({group, GroupId}),
    Thread = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_CALL_SERVICE
                                         , ?ARGUMENTS => #{ ?SERVICE_ID => BridgeId
                                                          , ?SERVICE_ACTION => get_function_id()
                                                          , ?SERVICE_CALL_VALUES => [#{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                      , ?VALUE => NonSharedValue
                                                                                      }]
                                                          }
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    Ret = automate_bot_engine_operations:run_thread(Thread, {?SIGNAL_PROGRAM_TICK, none}, undefined),
    Bridge ! done,
    ?assertMatch({stopped, _}, Ret).

allow_to_make_calls_on_shared_resource_values_multiple_resources() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-31-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-31-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-31-group">>,
    ResourceName1 = <<"channels">>,
    ResourceName2 = <<"groups">>,
    SharedValue1 = <<"shared-val-id1">>,
    SharedValue2 = <<"shared-val-id2">>,
    NonSharedValue = <<"non-shared-val-id">>,
    SharedValueName1 = <<"shared-val-name">>,
    SharedValueName2 = <<"shared-val-name">>,
    ReturnMessage = #{ <<"result">> => <<"ok">>} ,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-3-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName1}, {resource, ResourceName2}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),

    Bridge = test_bridge(BridgeId),
    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName1
                                                         , #{ SharedValue1 =>
                                                                  #{ <<"name">> => SharedValueName1
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName2
                                                         , #{ SharedValue2 =>
                                                                  #{ <<"name">> => SharedValueName2
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    {ok, ProgramId} =  ?BOT_UTILS:create_user_program({group, GroupId}),
    Thread = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_CALL_SERVICE
                                         , ?ARGUMENTS => #{ ?SERVICE_ID => BridgeId
                                                          , ?SERVICE_ACTION => get_function_id()
                                                          , ?SERVICE_CALL_VALUES => [ #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => SharedValue1
                                                                                       }
                                                                                    , #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => SharedValue2
                                                                                       }
                                                                                    ]
                                                          }
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    ?assertMatch({ran_this_tick, NewThreadState, _}, automate_bot_engine_operations:run_thread(Thread, {?SIGNAL_PROGRAM_TICK, none}, undefined)).

disallow_calls_on_non_shared_resource_values_multiple_resources() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-41-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-41-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-41-group">>,
    ResourceName1 = <<"channels">>,
    ResourceName2 = <<"groups">>,
    SharedValue1 = <<"shared-val-id1">>,
    SharedValue2 = <<"shared-val-id2">>,
    NonSharedValue = <<"non-shared-val-id">>,
    SharedValueName1 = <<"shared-val-name">>,
    SharedValueName2 = <<"shared-val-name">>,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-41-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName1}, {resource, ResourceName2}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    Bridge = test_bridge(BridgeId),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName1
                                                         , #{ SharedValue1 =>
                                                                  #{ <<"name">> => SharedValueName1
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName2
                                                         , #{ SharedValue2 =>
                                                                  #{ <<"name">> => SharedValueName2
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),
    {ok, ProgramId} =  ?BOT_UTILS:create_user_program({group, GroupId}),


    Thread1 = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_CALL_SERVICE
                                         , ?ARGUMENTS => #{ ?SERVICE_ID => BridgeId
                                                          , ?SERVICE_ACTION => get_function_id()
                                                          , ?SERVICE_CALL_VALUES => [ #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => SharedValue1
                                                                                       }
                                                                                    , #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => NonSharedValue
                                                                                       }
                                                                                    ]
                                                          }
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    Ret = automate_bot_engine_operations:run_thread(Thread1, {?SIGNAL_PROGRAM_TICK, none}, undefined),
    ?assertMatch({stopped, _}, Ret),

    Thread2 = #program_thread{ position = [1]
                            , program=[ #{ ?TYPE => ?COMMAND_CALL_SERVICE
                                         , ?ARGUMENTS => #{ ?SERVICE_ID => BridgeId
                                                          , ?SERVICE_ACTION => get_function_id()
                                                          , ?SERVICE_CALL_VALUES => [ #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => NonSharedValue
                                                                                       }
                                                                                    , #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                                       , ?VALUE => SharedValue2
                                                                                       }
                                                                                    ]
                                                          }
                                         }
                                      ]
                            , global_memory=#{}
                            , instruction_memory=#{}
                            , program_id=ProgramId
                            , thread_id=undefined
                            },

    Ret = automate_bot_engine_operations:run_thread(Thread2, {?SIGNAL_PROGRAM_TICK, none}, undefined),
    Bridge ! done,
    ?assertMatch({stopped, _}, Ret).


allow_to_listen_on_shared_resource_values() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-5-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-5-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-5-group">>,
    ResourceName = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,
    ReturnMessage = #{ <<"result">> => <<"ok">>} ,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-5-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    Bridge = test_bridge(BridgeId),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ok = automate_service_registry_query:listen_service(BridgeId, {group, GroupId}, { ResourceName, SharedValue }),
    ok = ?APPLICATION:from_service_port(BridgeId, {group, GroupId},
                                        #{ <<"type">> => <<"NOTIFICATION">>
                                         , <<"key">> => ResourceName
                                         , <<"to_user">> => ConnectionId
                                         , <<"value">> => test
                                         , <<"content">> => test
                                         , <<"subkey">> => SharedValue
                                         }),
    receive {channel_engine, _ChannelId, Msg} ->
            ?assertMatch(#{ <<"subkey">> := SharedValue }, Msg)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.


disallow_to_listen_on_non_shared_resources() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-6-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-6-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-6-group">>,
    ResourceName = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,
    ReturnMessage = #{ <<"result">> => <<"ok">>} ,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-6-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    Bridge = test_bridge(BridgeId),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ?assertMatch({error, no_valid_connection}, automate_service_registry_query:listen_service(BridgeId, {group, GroupId}, { ResourceName, undefined })).

disallow_to_listen_on_shared_resource_different_subkey() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-7-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-7-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-7-group">>,
    ResourceName = <<"channels">>,
    NonSharedValue = <<"non-shared-val-id">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,
    ReturnMessage = #{ <<"result">> => <<"ok">>} ,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-7-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    Bridge = test_bridge(BridgeId),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ?assertMatch({error, no_valid_connection}, automate_service_registry_query:listen_service(BridgeId, {group, GroupId}, { ResourceName, NonSharedValue })).

listening_on_shared_does_not_receive_different_subkeys() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-8-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-8-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-8-group">>,
    ResourceName = <<"channels">>,
    NonSharedValue = <<"non-shared-val-id">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,
    ReturnMessage = #{ <<"result">> => <<"ok">>} ,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-8-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    Bridge = test_bridge(BridgeId),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ok = automate_service_registry_query:listen_service(BridgeId, {group, GroupId}, { ResourceName, SharedValue }),

    ok = ?APPLICATION:from_service_port(BridgeId, {group, GroupId},
                                        #{ <<"type">> => <<"NOTIFICATION">>
                                         , <<"key">> => ResourceName
                                         , <<"to_user">> => ConnectionId
                                         , <<"value">> => <<"test">>
                                         , <<"content">> => <<"test">>
                                         , <<"subkey">> => NonSharedValue
                                         }),

    ok = ?APPLICATION:from_service_port(BridgeId, {group, GroupId},
                                        #{ <<"type">> => <<"NOTIFICATION">>
                                         , <<"key">> => ResourceName
                                         , <<"to_user">> => ConnectionId
                                         , <<"value">> => <<"test">>
                                         , <<"content">> => <<"test">>
                                         , <<"subkey">> => SharedValue
                                         }),
    receive {channel_engine, _, Msg1} ->
            ?assertMatch(#{ <<"subkey">> := SharedValue }, Msg1)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,
    receive {channel_engine, _, Msg2} ->
            ct:fail("Should only receive a message, received extra: " ++ lists:flatten(io_lib:format("~p", [Msg2])))
    after 0 ->
            ok
    end.


listening_on_shared_does_not_receive_null_subkeys() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-9-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-9-reader">>,
    GroupName = <<?TEST_ID_PREFIX, "-test-9-group">>,
    ResourceName = <<"channels">>,
    SharedValue = <<"shared-val-id">>,
    SharedValueName = <<"shared-val-name">>,
    ReturnMessage = #{ <<"result">> => <<"ok">>} ,

    {ok, #user_group_entry{ id=GroupId }} = automate_storage:create_group(GroupName, OwnerUser, false),
    ok = automate_storage:add_collaborators({ group, GroupId }, [{ReaderUserId, editor}]),

    BridgeName = <<?TEST_ID_PREFIX, "-test-9-service-port">>,
    {ok, BridgeId} = ?APPLICATION:create_service_port(OwnerUser, BridgeName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => BridgeName
                     , <<"blocks">> => [ get_test_block([{resource, ResourceName}]) ]
                     },
    ok = ?APPLICATION:from_service_port(BridgeId, OwnerUser,
                                        #{ <<"type">> => <<"CONFIGURATION">>
                                         , <<"value">> => Configuration
                                         }),

    {ok, ConnectionId} = ?UTILS:establish_connection(BridgeId, OwnerUser),
    Bridge = test_bridge(BridgeId),

    ok = automate_service_port_engine:set_shared_resource(ConnectionId
                                                         , ResourceName
                                                         , #{ SharedValue =>
                                                                  #{ <<"name">> => SharedValueName
                                                                   , <<"shared_with">> => [ #{ <<"id">> => GroupId
                                                                                             , <<"type">> => <<"group">>
                                                                                             }
                                                                                          ] } }),

    ok = automate_service_registry_query:listen_service(BridgeId, {group, GroupId}, { ResourceName, SharedValue }),

    ok = ?APPLICATION:from_service_port(BridgeId, {group, GroupId},
                                        #{ <<"type">> => <<"NOTIFICATION">>
                                         , <<"key">> => ResourceName
                                         , <<"to_user">> => ConnectionId
                                         , <<"value">> => <<"test">>
                                         , <<"content">> => <<"test">>
                                         , <<"subkey">> => null
                                         }),

    ok = ?APPLICATION:from_service_port(BridgeId, {group, GroupId},
                                        #{ <<"type">> => <<"NOTIFICATION">>
                                         , <<"key">> => ResourceName
                                         , <<"to_user">> => ConnectionId
                                         , <<"value">> => <<"test">>
                                         , <<"content">> => <<"test">>
                                         , <<"subkey">> => SharedValue
                                         }),
    receive {channel_engine, _, Msg1} ->
            ?assertMatch(#{ <<"subkey">> := SharedValue }, Msg1)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,
    receive {channel_engine, _, Msg2} ->
            ct:fail("Should only receive a message, received extra: " ++ lists:flatten(io_lib:format("~p", [Msg2])))
    after 0 ->
            ok
    end.


%%====================================================================
%% Custom block tests - Internal functions
%%====================================================================
-define(FunctionName, <<"first_function_name">>).
-define(FunctionId, <<"first_function_id">>).
-define(FunctionMessage, <<"sample message">>).
-define(BlockType, <<"str">>).
-define(BlockResultType, null).
-define(SaveTo, undefined).

get_function_id() ->
    ?FunctionId.

build_arguments(Args) ->
    lists:map(fun(Arg) ->
                      case Arg of
                          {resource, Name} ->
                              #{ <<"type">> => <<"string">>
                               , <<"values">> => #{ <<"collection">> => Name }
                               };
                          Num when is_number(Num) ->
                              #{ <<"type">> => <<"integer">>
                               , <<"default">> => integer_to_binary(Num)
                               }
                      end
              end, Args).

get_test_block(Arguments) ->
    #{ <<"arguments">> => build_arguments(Arguments)
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
                                    , arguments=_
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

test_bridge(BridgeId) ->
    Orig = self(),
    BridgePid = spawn(fun() ->
                           ok = ?ROUTER:connect_bridge(BridgeId),
                           Orig ! ready,
                           receive
                               { automate_service_port_engine_router
                               , _ %% From
                               , { data, MessageId, RecvMessage }} ->
                                   ok = ?ROUTER:answer_message(MessageId, #{ <<"result">> => RecvMessage });
                               done ->
                                   ok
                           end
                   end),
    receive ready -> ok end,
    BridgePid.
