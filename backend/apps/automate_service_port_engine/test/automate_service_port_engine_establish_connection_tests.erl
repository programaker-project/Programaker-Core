%%% @doc
%%% Automate service port custom blocks management tests.
%%% @end

-module(automate_service_port_engine_establish_connection_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").

%% Test data
-define(APPLICATION, automate_service_port_engine).
-define(TEST_NODES, [node()]).
-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(TEST_ID_PREFIX, "automate_service_port_engine_tests").
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
    [ { "[Service Port - Establish connection] Establish connection with owner user"
      , fun establish_connection_with_owner_user/0
      }
    , { "[Service Port - Establish connection] Establish connection with owner group"
      , fun establish_connection_with_owner_group/0
      }
    ].


%%====================================================================
%% Connection tests
%%====================================================================
establish_connection_with_owner_user() ->
    OwnerUserId = {user, <<?TEST_ID_PREFIX, "-test-1-owner">>},
    ServicePortName = <<?TEST_ID_PREFIX, "-test-1-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => []
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

    ok = establish_connection(ServicePortId, OwnerUserId),

    {ok, [ _Connection ]} = ?APPLICATION:list_established_connections(OwnerUserId).

establish_connection_with_owner_group() ->
    OwnerUserId = {group, <<?TEST_ID_PREFIX, "-test-2-owner">>},
    ServicePortName = <<?TEST_ID_PREFIX, "-test-2-service-port">>,
    {ok, ServicePortId} = ?APPLICATION:create_service_port(OwnerUserId, ServicePortName),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => ServicePortName
                     , <<"blocks">> => []
                     },
    ok = ?APPLICATION:from_service_port(ServicePortId, OwnerUserId,
                                        jiffy:encode(#{ <<"type">> => <<"CONFIGURATION">>
                                                      , <<"value">> => Configuration
                                                      })),

    ok = establish_connection(ServicePortId, OwnerUserId),

    {ok, [ _Connection ]} = ?APPLICATION:list_established_connections(OwnerUserId).



%%====================================================================
%% Auxiliary functions
%%====================================================================
establish_connection(ServicePortId, Owner) ->
    Orig = self(),
    Server = spawn(fun() ->
                           ok = automate_service_port_engine:register_service_port(ServicePortId),
                           receive
                               {automate_service_port_engine_router, _Pid, {data, MessageId, #{ <<"type">> := <<"GET_HOW_TO_SERVICE_REGISTRATION">> }}} ->
                                   Anwser = ?APPLICATION:from_service_port(ServicePortId, Owner,
                                                                           jiffy:encode(#{ message_id => MessageId
                                                                                         , success => true
                                                                                         , result => #{ type => message
                                                                                                      , value => #{ form => []}}
                                                                                         })),
                                   io:fwrite("\033[41;37;1m Answer: ~p \033[0m~n", [Anwser])
                           end,
                           receive {connect, ConnectionId} ->
                                   ?APPLICATION:from_service_port(ServicePortId, Owner,
                                                                  jiffy:encode(#{ type => <<"ESTABLISH_CONNECTION">>
                                                                                , value => #{ connection_id => ConnectionId
                                                                                            , name => ?MODULE
                                                                                            }
                                                                                }))
                           end,
                           Orig ! done
                   end),

    {ok, #{module := Module}} = automate_service_registry:get_service_by_id(ServicePortId),
    {ok, HowTo } = automate_service_registry_query:get_how_to_enable(Module,
                                                                     case Owner of
                                                                         {user, UserId} -> #{ user_id => UserId };
                                                                         {group, GroupId} -> #{ group_id => GroupId }
                                                                     end),

    case HowTo of
        #{ <<"type">> := <<"message">>, <<"connection_id">> := ConnectionId } ->
            Server ! {connect, ConnectionId},
            receive done -> ok end
    end.
