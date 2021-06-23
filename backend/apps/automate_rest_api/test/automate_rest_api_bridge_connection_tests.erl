%%% Automate bridge token creation, API tests.
%%% @end

-module(automate_rest_api_bridge_connection_tests).
-include_lib("eunit/include/eunit.hrl").

%% Data structures
-include("../../automate_storage/src/records.hrl").

-define(APPLICATION, automate_rest_api).
-define(PREFIX, "automate_rest_api_token_tests_").
-define(RECEIVE_TIMEOUT, 100).
-define(BRIDGE_BACKEND, automate_service_port_engine_mnesia_backend).

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

    Port = get_test_port(),
    application:set_env(automate_rest_api, port, Port, [{persistent, true}]),

    %% Use a custom node name to avoid overwriting the actual databases
    net_kernel:start([testing, shortnames]),

    {ok, _} = application:ensure_all_started(automate_storage),
    {ok, _} = application:ensure_all_started(automate_service_port_engine),
    {ok, _} = application:ensure_all_started(automate_channel_engine),
    {ok, _} = application:ensure_all_started(?APPLICATION),

    {Port, NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({_Port, _NodeName}) ->
    ok = application:stop(?APPLICATION),

    ok.


tests({Port, _}) ->
    %% Token creation
    [ { "[API - Bridges - Detect when connection is established] Detect connection and disconnection of bridge"
      , fun() -> detect_bridge_connection_disconnection(Port) end
      }
    , { "[API - Bridges - Detect when connection is established] Interlocking connection-disconnection"
      , fun() -> detect_bridge_connection_disconnection_interlocking(Port) end
      }
    ].


%%====================================================================
%% Tests
%%====================================================================
detect_bridge_connection_disconnection(Port) ->
    #{ user_id := UserId
     , bridge_id := BridgeId
     , bridge_token := BridgeToken
     } = create_bridge_and_token(Port),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => "Bridge connection detection test - 1"
                     , <<"blocks">> => [ ]
                     },

    ok = automate_service_port_engine:from_service_port(BridgeId, {user, UserId},
                                                        #{ <<"type">> => <<"CONFIGURATION">>
                                                         , <<"value">> => Configuration
                                                         }),

    {ok, _ConnectionId} = establish_connection(BridgeId, {user, UserId}),

    ok = automate_service_registry_query:listen_service(BridgeId, {user, UserId}, {undefined, undefined}),

    AuthMessage = jiffy:encode(#{ <<"type">> => <<"AUTHENTICATION">>
                                , <<"value">> => #{ <<"token">> => BridgeToken
                                                  }
                                }),

    %% Connect
    {ok, AuthState={state, _, _, _, true}} = automate_rest_api_service_ports_specific_communication:websocket_handle(
                                               {text, AuthMessage}, { state, {user, UserId}, BridgeId, #{}, false }),

    receive
        { channel_engine
        , _ChannelId
        , #{ <<"key">> := '__proto_on_bridge_connected'
           , <<"service_id">> := BridgeId
           , <<"subkey">> := BridgeId
           , <<"value">> := <<"connected">>
           }
        } ->
            ok
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,

    %% Disconnect
    ok = automate_rest_api_service_ports_specific_communication:terminate(test, idk, AuthState),

    receive
        { channel_engine
        , _ChannelId2
        , #{ <<"key">> := '__proto_on_bridge_disconnected'
           , <<"service_id">> := BridgeId
           , <<"subkey">> := BridgeId
           , <<"value">> := <<"disconnected">>
           }
        } ->
            ok
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,

    ok.

detect_bridge_connection_disconnection_interlocking(Port) ->
    #{ user_id := UserId
     , bridge_id := BridgeId
     , bridge_token := BridgeToken
     } = create_bridge_and_token(Port),

    Configuration = #{ <<"is_public">> => false
                     , <<"service_name">> => "Bridge connection detection test - 1"
                     , <<"blocks">> => [ ]
                     },

    ok = automate_service_port_engine:from_service_port(BridgeId, {user, UserId},
                                                        #{ <<"type">> => <<"CONFIGURATION">>
                                                         , <<"value">> => Configuration
                                                         }),

    {ok, _ConnectionId} = establish_connection(BridgeId, {user, UserId}),

    ok = automate_service_registry_query:listen_service(BridgeId, {user, UserId}, {undefined, undefined}),

    AuthMessage = jiffy:encode(#{ <<"type">> => <<"AUTHENTICATION">>
                                , <<"value">> => #{ <<"token">> => BridgeToken
                                                  }
                                }),

    %% Connections are handled on different processes so the router can differentiate them
    %% First connection, on a different PID so the router can differentiate between the two
    Conn1 = spawn(
                   fun() ->
                           {ok, AuthState2={state, _, _, _, true}} = automate_rest_api_service_ports_specific_communication:websocket_handle(
                                                                       {text, AuthMessage}, { state, {user, UserId}, BridgeId, #{}, false }),
                           receive continue -> ok end,
                           %% Disconnect
                           ok = automate_rest_api_service_ports_specific_communication:terminate(test, idk, AuthState2)
                   end),


    %% First connection triggers a connection event
    receive
        { channel_engine
        , _ChannelId
        , #{ <<"key">> := '__proto_on_bridge_connected'
           , <<"service_id">> := BridgeId
           , <<"subkey">> := BridgeId
           , <<"value">> := <<"connected">>
           }
        } ->
            ok
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,

    %% Second connection, on a different PID so the router can differentiate between the two
    Conn2 = spawn(
                   fun() ->
                           {ok, AuthState2={state, _, _, _, true}} = automate_rest_api_service_ports_specific_communication:websocket_handle(
                                                                       {text, AuthMessage}, { state, {user, UserId}, BridgeId, #{}, false }),
                           receive continue -> ok end,
                           %% Disconnect
                           ok = automate_rest_api_service_ports_specific_communication:terminate(test, idk, AuthState2)
                   end),

    %% Second connnection does NOT trigger a connection event
    receive
        { channel_engine
        , _ChannelId2
        , #{ <<"key">> := '__proto_on_bridge_connected'
           , <<"service_id">> := BridgeId
           , <<"subkey">> := BridgeId
           , <<"value">> := <<"connected">>
           }
        } ->
            ct:fail(should_not_happen)
    after ?RECEIVE_TIMEOUT ->
            ok
    end,

    Conn1 ! continue,

    %% First disconnection does NOT trigger a disconnection event
    receive
        { channel_engine
        , _ChannelId3
        , #{ <<"key">> := '__proto_on_bridge_disconnected'
           , <<"service_id">> := BridgeId
           , <<"subkey">> := BridgeId
           , <<"value">> := <<"disconnected">>
           }
        } ->
            ct:fail(should_not_happen)
    after ?RECEIVE_TIMEOUT ->
            ok
    end,

    %% Second disconnection
    Conn2 ! continue,

    %% Second disconnection does trigger a disconnection event
    receive
        { channel_engine
        , _ChannelId4
        , #{ <<"key">> := '__proto_on_bridge_disconnected'
           , <<"service_id">> := BridgeId
           , <<"subkey">> := BridgeId
           , <<"value">> := <<"disconnected">>
           }
        } ->
            ok
    after ?RECEIVE_TIMEOUT ->
            ct:fail(timeout)
    end,

    ok.


%%====================================================================
%% Utils
%%====================================================================
create_bridge_and_token(Port) ->
    Uuid = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    TokenName = <<"simple-token">>,
    Username = <<?PREFIX, "user", Uuid/binary>>,
    Password = <<?PREFIX, "pass">>,
    Mail = <<?PREFIX, "mail",  Uuid/binary, "@mail.com">>,
    BridgeName = <<?PREFIX, "bridge">>,
    {ok, UserId} = automate_storage:create_user(Username, Password, Mail, ready),
    {ok, BridgeId} = automate_service_port_engine:create_service_port({user, UserId}, BridgeName),
    {ok, {UserToken, UserId} } = automate_storage:login_user(Username, Password),

    Uri = fmt_list("http://localhost:~p/api/v0/bridges/by-id/~s/tokens", [ Port, BridgeId ]),
    io:fwrite("Uri: ~p~n", [Uri]),
    Result = httpc:request(post
                          , { Uri
                            , [ { "Authorization", binary:bin_to_list(UserToken) } ]
                            , "application/json"
                            , jiffy:encode(#{ name => TokenName })
                            }
                          , [], []),

    io:fwrite("Result: ~p~n", [Result]),
    {ok, {{_, 201, _}, _Headers, Body}} = Result,

    io:fwrite("Body: ~p~n", [Body]),
    #{ <<"key">> := BridgeToken } = jiffy:decode(Body, [return_maps]),
    #{ user_id => UserId
     , bridge_id => BridgeId
     , bridge_token => BridgeToken
     }.

fmt_list(FmtStr, Params) ->
    lists:flatten(io_lib:format(FmtStr, Params)).

get_test_port() ->
    12345.

establish_connection(BridgeId, UserId) ->
    case ?BRIDGE_BACKEND:gen_pending_connection(BridgeId, UserId) of
        {ok, ConnectionId} ->
            ok = ?BRIDGE_BACKEND:establish_connection(BridgeId, UserId, ConnectionId, <<"test connection">>),
            {ok, ConnectionId};
        {error, Reason} ->
            {error, Reason}
    end.
