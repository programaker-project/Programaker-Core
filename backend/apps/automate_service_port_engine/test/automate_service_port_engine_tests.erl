%%% @doc
%%% Automate service port custom blocks management tests.
%%% @end

-module(automate_service_port_engine_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").

%% Test data
-define(APPLICATION, automate_service_port_engine).
-define(TEST_NODES, [node()]).
-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(TEST_ID_PREFIX, "automate_service_port_engine_custom_blocks_tests").
-define(RECEIVE_TIMEOUT, 100).

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

    {ok, _Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({NodeName}) ->
    ?BACKEND:uninstall(),
    ok = application:stop(?APPLICATION),

    %% Restore the original node name
    net_kernel:start([NodeName, shortnames]),
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
    , { "[Service Port - Custom blocks] Non-owned public blocks appear"
      , fun non_owned_public_blocks_appear/0
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
    ].


%%====================================================================
%% Custom block tests
%%====================================================================
owned_private_blocks_appear() ->
    OwnerUserId = <<?TEST_ID_PREFIX, "-test-1-owner">>,
    ServicePortName = <<?TEST_ID_PREFIX, "-test-1-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),
    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(OwnerUserId),

    check_test_block(CustomBlock).


non_owned_private_blocks_dont_appear() ->
    OwnerUserId = <<?TEST_ID_PREFIX, "-test-2-owner">>,
    RequesterUserId = <<?TEST_ID_PREFIX, "-test-2-requester">>,
    ServicePortName = <<?TEST_ID_PREFIX, "-test-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),
    {ok, Results} = ?APPLICATION:list_custom_blocks(RequesterUserId),
    ?assertEqual(#{}, Results).


owned_public_blocks_appear() ->
    OwnerUserId = <<?TEST_ID_PREFIX, "-test-3-owner">>,
    ServicePortName = <<?TEST_ID_PREFIX, "-test-3-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => public
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),
    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(OwnerUserId),

    check_test_block(CustomBlock).

non_owned_public_blocks_appear() ->
    OwnerUserId = <<?TEST_ID_PREFIX, "-test-4-owner">>,
    RequesterUserId = <<?TEST_ID_PREFIX, "-test-4-requester">>,
    ServicePortName = <<?TEST_ID_PREFIX, "-test-4-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ get_test_block() ]
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),
    {ok, #{ ServicePortId := [CustomBlock] }} = ?APPLICATION:list_custom_blocks(RequesterUserId),

    check_test_block(CustomBlock).


%%====================================================================
%% Custom block tets - Internal functions
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
    OwnerUserId = <<?TEST_ID_PREFIX, "-route-test-1-owner">>,
    TargetUserId = OwnerUserId,
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-1-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

    %% Listen on the service port
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServicePortId,
                                                                              TargetUserId),
    {ok, ChannelId } = automate_service_registry_query:get_monitor_id(Module, TargetUserId),
    ok = automate_channel_engine:listen_channel(ChannelId),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              TargetUserId, #{ <<"test">> => 1 }),

    %% Catch notification
    receive {channel_engine, ChannelId, ReceivedMessage} ->
            ?assertEqual(ExpectedContent, ReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.


route_notification_targeted_to_owner_on_public() ->
    OwnerUserId = <<?TEST_ID_PREFIX, "-route-test-2-owner">>,
    TargetUserId = OwnerUserId,
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

    %% Listen on the service port
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServicePortId,
                                                                              TargetUserId),
    {ok, ChannelId } = automate_service_registry_query:get_monitor_id(Module, TargetUserId),
    ok = automate_channel_engine:listen_channel(ChannelId),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              TargetUserId, #{ <<"test">> => 2 }),

    %% Catch notification
    receive {channel_engine, ChannelId, ReceivedMessage} ->
            ?assertEqual(ExpectedContent, ReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.

route_notification_targeted_to_non_owner_on_public() ->
    OwnerUserId = <<?TEST_ID_PREFIX, "-route-test-3-owner">>,
    TargetUserId = <<?TEST_ID_PREFIX, "-route-test-3-NONowner-TARGET">>,
    ServicePortName = <<?TEST_ID_PREFIX, "-route-test-3-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    %% Configure service port
    Configuration = #{ <<"is_public">> => true
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => [ ]
                     },

    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

    %% Listen on the service port
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServicePortId,
                                                                              TargetUserId),
    {ok, ChannelId } = automate_service_registry_query:get_monitor_id(Module, TargetUserId),
    ok = automate_channel_engine:listen_channel(ChannelId),

    %% Emit notification
    {ok, ExpectedContent} = emit_notification(ServicePortId, OwnerUserId,
                                              TargetUserId, #{ <<"test">> => 3 }),

    %% Catch notification
    receive {channel_engine, ChannelId, ReceivedMessage} ->
            ?assertEqual(ExpectedContent, ReceivedMessage)
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end.

%%====================================================================
%% Notification routing tests - Internal functions
%%====================================================================
emit_notification(ServicePortId, OwnerUserId, TargetUserId, Content) ->
    Key = binary:list_to_bin(atom_to_list(?MODULE)),
    Value = Content, %% For simplicity
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"NOTIFICATION">>
                                                      , <<"key">> => Key
                                                      , <<"to_user">> => TargetUserId
                                                      , <<"value">> => Value
                                                      , <<"content">> => Content
                                                      })),
    {ok, #{ <<"content">> => Content
          , <<"key">> => Key
          , <<"to_user">> => TargetUserId
          , <<"value">> => Value
          }}.
