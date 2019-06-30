%%% @doc
%%% Automate service port router tests.
%%% @end

-module(automate_service_port_engine_router_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test data
-define(APPLICATION, automate_service_port_engine).
-define(ROUTER, automate_service_port_engine_router).
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

    {ok, _Pid} = application:ensure_all_started(?APPLICATION),

    {NodeName}.

%% @doc App infrastructure teardown.
%% @end
stop({NodeName}) ->
    ok = application:stop(?APPLICATION),

    %% Restore the original node name
    net_kernel:start([NodeName, shortnames]),
    ok.

tests(_SetupResult) ->
    %% Routing
    [ {"[Service Port Router] Route one-to-one", fun route_one_to_one/0}
    , {"[Service Port Router] Route two-to-one", fun route_two_to_one/0}
    , {"[Service Port Router] Route one-to-two", fun route_one_to_two/0}
    , {"[Service Port Router] Route one-to-zero", fun route_one_to_zero/0}
    , {"[Service Port Router] Route two-to-zero", fun route_two_to_zero/0}
    ].

%%%% Routing
route_one_to_one() ->
    BridgeId = <<"3066b6d6-4462-45f6-b897-52555d5dbebf">>,
    Message = #{ value => sample },
    ReturnMessage = #{ value => ok },
    Orig = self(),
    spawn_link(fun() ->
                       ok = ?ROUTER:connect_bridge(BridgeId),
                       Orig ! ready,
                       receive
                           Data = { automate_service_port_engine_router
                           , _From
                           , { data, MessageId, RecvMessage }} ->
                               ?assertMatch(Message, RecvMessage),

                               ok = ?ROUTER:answer_message(MessageId, ReturnMessage);
                           Fail ->
                               ct:fail(timeout2)
                       end
               end),
    receive ready -> ok end,
    {ok, Result} = ?ROUTER:call_bridge(BridgeId, Message),
    ?assertMatch(ReturnMessage, Result).

route_two_to_one() ->
    BridgeId = <<"af8618e9-2078-443b-9834-e23180c1673a">>,
    Message1 = #{ value => sample1 },
    Message2 = #{ value => sample2 },
    ReturnMessage1 = #{ value => ok1 },
    ReturnMessage2 = #{ value => ok2 },
    Orig = self(),
    spawn_link(fun() ->
                       ok = ?ROUTER:connect_bridge(BridgeId),
                       Orig ! ready,
                       receive
                           { automate_service_port_engine_router
                           , _ %% From
                           , { data, MessageId1, RecvMessage1 }} ->
                               ?assertMatch(Message1, RecvMessage1),
                               ok = ?ROUTER:answer_message(MessageId1, ReturnMessage1);
                           _ ->
                               ct:fail(timeout)
                       end,
                       receive
                           { automate_service_port_engine_router
                           , _ %% From
                           , { data, MessageId2, RecvMessage2 }} ->
                               ?assertMatch(Message2, RecvMessage2),
                               ok = ?ROUTER:answer_message(MessageId2, ReturnMessage2);
                           _ ->
                               ct:fail(timeout)
                       end
               end),
    receive ready -> ok end,
    {ok, Result1} = ?ROUTER:call_bridge(BridgeId, Message1),
    ?assertMatch(ReturnMessage1, Result1),
    {ok, Result2} = ?ROUTER:call_bridge(BridgeId, Message2),
    ?assertMatch(ReturnMessage2, Result2).

route_one_to_two() ->
    BridgeId = <<"5d606167-9968-4581-8d6c-f2340f35d065">>,
    Message = #{ value => sample },
    Orig = self(),

    Listener = (fun() ->
                        ok = ?ROUTER:connect_bridge(BridgeId),
                        Orig ! ready,
                        receive
                            { automate_service_port_engine_router
                            , _ %% From
                            , { data, MessageId, RecvMessage }} ->
                                ?assertMatch(Message, RecvMessage),
                                ok = ?ROUTER:answer_message(MessageId, self());
                            close ->
                                Orig ! closed
                        end
                end),

    First = spawn_link(Listener),
    Second = spawn_link(Listener),
    receive ready -> ok end,
    receive ready -> ok end,
    {ok, Catcher} = ?ROUTER:call_bridge(BridgeId, Message),
    ToClose = case Catcher of
                  First -> Second;
                  Second -> First
              end,
    ToClose ! close,
    receive closed -> ok end.


route_one_to_zero() ->
    BridgeId = <<"6958a5cd-1559-42c1-a628-4ed2cb8d3d5e">>,
    Message = #{ value => sample },
    {error, no_connection} = ?ROUTER:call_bridge(BridgeId, Message).

route_two_to_zero() ->
    BridgeId = <<"9875dcd5-2bcc-414d-aa4a-4f2f7709332d">>,
    Message = #{ value => sample },
    {error, no_connection} = ?ROUTER:call_bridge(BridgeId, Message),
    {error, no_connection} = ?ROUTER:call_bridge(BridgeId, Message).

