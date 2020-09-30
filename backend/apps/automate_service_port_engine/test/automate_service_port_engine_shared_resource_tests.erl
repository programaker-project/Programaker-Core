%%% @doc
%%% Automate service port shared resource management tests.
%%% @end

-module(automate_service_port_engine_shared_resource_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").
-include("../../automate_storage/src/records.hrl").

%% Test data
-define(APPLICATION, automate_service_port_engine).
-define(TEST_NODES, [node()]).
-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(TEST_ID_PREFIX, "automate_service_port_engine_shared_resource_tests").
-define(RECEIVE_TIMEOUT, 100).
-define(UTILS, automate_service_port_engine_test_utils).
-define(BOT_UTILS, automate_bot_engine_test_utils).

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
    , { "[Bridge - Shared resources] Allow to share resources, blocks that require a non-shared resource don't appear"
      , fun non_shared_resources_negate_custom_blocks/0
      }
      %% TODO: Should it show blocks with NO required resources?
    ].


%%====================================================================
%% Custom block tests
%%====================================================================
blocks_with_shared_resources_appear() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-1-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-1-reader">>,
    ReaderUser = {user, ReaderUserId},
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
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

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

non_shared_resources_negate_custom_blocks() ->
    OwnerUser = {user, <<?TEST_ID_PREFIX, "-test-2-owner">>},
    ReaderUserId = <<?TEST_ID_PREFIX, "-test-2-reader">>,
    ReaderUser = {user, ReaderUserId},
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
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

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


%%====================================================================
%% Custom block tests - Internal functions
%%====================================================================
-define(FunctionName, <<"first_function_name">>).
-define(FunctionId, <<"first_function_id">>).
-define(FunctionMessage, <<"sample message">>).
-define(BlockType, <<"str">>).
-define(BlockResultType, null).
-define(SaveTo, undefined).

build_arguments(Args) ->
    lists:map(fun(Arg) ->
                      case Arg of
                          {resource, Name} ->
                              #{ type => string
                               , values => #{ collection => Name }
                               };
                          Num when is_number(Num) ->
                              #{ type => integer
                               , default => integer_to_binary(Num)
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
