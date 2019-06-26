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
    ?debugFmt("\033[7mPreparing ~p\033[0m~n", [?APPLICATION]),
    net_kernel:start([?MODULE, shortnames]),

    {ok, _Pid} = application:ensure_all_started(?APPLICATION),
    ?debugMsg("Started, continuing~n"),

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
      %% , {"[Service Port Router] Route two-to-one", fun route_two_to_one/0}
      %% , {"[Service Port Router] Route one-to-two", fun route_one_to_two/0}
      %% , {"[Service Port Router] Route one-to-zero", fun route_one_to_zero/0}
      %% , {"[Service Port Router] Route two-to-zero", fun route_two_to_zero/0}
    ].

%%%% Routing
route_one_to_one() ->
    BridgeId = <<"3066b6d6-4462-45f6-b897-52555d5dbebf">>,
    Message = #{ value => sample },
    ReturnMessage = #{ value => ok },
    Orig = self(),
    ?debugMsg("Spawning~n"),
    spawn_link(fun() ->
                       ?debugMsg("Connecting~n"),
                       ok = ?ROUTER:connect_bridge(BridgeId),
                       ?debugMsg("Connected~n"),
                       Orig ! ready,
                       ?debugMsg("Waiting~n"),
                       receive
                           Data = { automate_service_port_engine_router
                           , _From
                           , { data, MessageId, RecvMessage }} ->
                               ?debugFmt("Gotcha! ~p", [Data]),
                               ?assertMatch(RecvMessage, Message),
                               ?ROUTER:answer_message(MessageId, ReturnMessage);
                           Fail ->
                               ?debugFmt("Fail: ~p", [Fail]),
                               ct:fail(timeout2)
                       end
               end),
    receive ready -> ok end,
    ?debugMsg("Calling~n"),
    {ok, Result} = ?ROUTER:call_bridge(BridgeId, Message),
    ?debugMsg("Finished~n"),
    ?assertMatch(Result, ReturnMessage).
