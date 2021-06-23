-module(automate_service_port_engine_router).

%% API
-export([ start_link/0
        , connect_bridge/1
        , disconnect_bridge/1
        , call_bridge/2
        , is_bridge_connected/1
        , answer_message/2
        ]).

-define(SERVER, ?MODULE).
-ifdef(TEST).
-define(MAX_WAIT_TIME_SECONDS, 1).
-else.
-define(MAX_WAIT_TIME_SECONDS, 100).
-endif.
-define(MAX_WAIT_TIME, ?MAX_WAIT_TIME_SECONDS * 1000).
-include("databases.hrl").
-include("records.hrl").
-include("router_error_cases.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Connect a bridge to the router.
%%
%% @spec connect_bridge(BridgeId :: binary()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
connect_bridge(BridgeId) ->
    Pid = self(),
    Node = node(),
    Transaction = fun() ->
                          ok = mnesia:write(?CONNECTED_BRIDGES_TABLE,
                                            #bridge_connection_entry{ id=BridgeId
                                                                    , pid=Pid
                                                                    , node=Node
                                                                    },
                                            write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Disconnect a bridge to the router.
%%
%% @spec connect_bridge(BridgeId :: binary()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
disconnect_bridge(BridgeId) ->
    Pid = self(),
    Node = node(),
    Transaction = fun() ->
                          ok = mnesia:delete_object(?CONNECTED_BRIDGES_TABLE,
                                                    #bridge_connection_entry{ id=BridgeId
                                                                            , pid=Pid
                                                                            , node=Node
                                                                            },
                                                    write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Send a call to a bridge and return the result.
%%
%% @spec call_bridge(BridgeId, Msg) -> {ok, Value} | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec call_bridge(binary(), map()) -> {ok, map()} | {error, ?ROUTER_ERROR_CLASSES}.
call_bridge(BridgeId, Msg) ->
    automate_stats:log_observation(counter,
                                   automate_bridge_engine_messages_to_bridge,
                                   [BridgeId]),

    case get_bridge_alive_connections(BridgeId) of
        {error, Error} ->
            {error, Error};
        {ok, []} ->
            {error, no_connection};
        {ok, L} ->
            Bridge = lists:nth(rand:uniform(length(L)), L),
            case call_bridge_to_connection(Bridge, Msg, self(), node()) of
                {ok, _MessageId} ->
                    wait_bridge_response();
                {error, Error} ->
                    {error, Error}
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Route a message answer.
%%
%% @spec answer_message(MessageId, Response) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
answer_message(MessageId, Response) ->
    case get_caller_pid(MessageId) of
        {error, Error} ->
            {error, Error};
        {ok, Entry=#on_flight_message_entry{pid=Pid}} ->
            Pid ! {?SERVER, {ok, Response}},
            ok = remove_on_flight_message(Entry),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check if a bridge has server connections.
%%
%% @spec is_bridge_connected(BridgeId) -> {ok, boolean} | {error, Error}
%% @end
%%--------------------------------------------------------------------
is_bridge_connected(BridgeId) ->
    case get_bridge_alive_connections(BridgeId) of
        {error, Error} ->
            {error, Error};
        {ok, []} ->
            {ok, false};
        {ok, _} ->
            {ok, true}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Nodes = automate_configuration:get_sync_peers(),
    %% Connected service ports
    ok = case mnesia:create_table(?CONNECTED_BRIDGES_TABLE,
                                  [ { attributes, record_info(fields, bridge_connection_entry)}
                                  , { ram_copies, Nodes }
                                  , { record_name, bridge_connection_entry }
                                  , { type, bag }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = case mnesia:create_table(?ON_FLIGHT_MESSAGES_TABLE,
                                  [ { attributes, record_info(fields, on_flight_message_entry)}
                                  , { ram_copies, Nodes }
                                  , { record_name, on_flight_message_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = mnesia:wait_for_tables([ ?CONNECTED_BRIDGES_TABLE
                                , ?ON_FLIGHT_MESSAGES_TABLE
                                ], automate_configuration:get_table_wait_time()),
    ignore.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a message to all bridges in a list.
%%
%% @spec send_to_all(msg, bridges) -> {ok, ContinuingBridges} | { error, Error }
%% @end
%%--------------------------------------------------------------------
-spec call_bridge_to_connection(#bridge_connection_entry{},
                                any(), pid(), node()) -> { ok, binary()}
                                                             | {error, _}.
call_bridge_to_connection(#bridge_connection_entry{pid=Pid}, Msg, From, FromNode) ->
    MessageId = generate_id(),
    Transaction = fun() ->
                          ok = mnesia:write(?ON_FLIGHT_MESSAGES_TABLE,
                                            #on_flight_message_entry{ message_id=MessageId
                                                                    , pid=From
                                                                    , node=FromNode
                                                                    },
                                            write)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            Pid ! { ?SERVER, From, { data, MessageId, Msg }},
            {ok, MessageId};
        {aborted, Error} ->
            {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec generate_id() -> binary().
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).


-spec get_caller_pid(binary()) -> {ok, #on_flight_message_entry{}} | {error, _}.
get_caller_pid(MessageId) ->
    Transaction = fun() ->
                          case mnesia:read(?ON_FLIGHT_MESSAGES_TABLE, MessageId) of
                              [] -> {error, no_message_id};
                              [OnFlightEntry] -> {ok, OnFlightEntry}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Error} ->
            {error, Error}
    end.


-spec remove_on_flight_message(#on_flight_message_entry{}) -> ok.
remove_on_flight_message(Entry) ->
    mnesia:dirty_delete_object(?ON_FLIGHT_MESSAGES_TABLE, Entry).


-spec get_bridge_alive_connections(binary()) -> {ok, [#bridge_connection_entry{}]}
                                                    | {error, any()}.
get_bridge_alive_connections(BridgeId) ->
    Transaction = fun() -> %% Maybe a dirty read is enough here
                          mnesia:read(?CONNECTED_BRIDGES_TABLE, BridgeId)
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            { AliveConnections, DeadConnections } = check_alive_connections(Result),
            drop_dead_connections(DeadConnections),
            {ok, AliveConnections};
        {aborted, Error} ->
            {error, Error}
    end.


-spec drop_dead_connections([#bridge_connection_entry{}]) -> ok.
drop_dead_connections(DeadConnections) ->
    lists:foreach(fun(DeadConnection) ->
                          mnesia:dirty_delete_object(?CONNECTED_BRIDGES_TABLE, DeadConnection)
                  end, DeadConnections).


-spec check_alive_connections([#bridge_connection_entry{}]) -> { [#bridge_connection_entry{}]
                                                               , [#bridge_connection_entry{}]
                                                               }.
check_alive_connections(Connections) ->
    lists:partition(fun(#bridge_connection_entry{pid=Pid, node=Node }) ->
                            automate_coordination_utils:is_process_alive(Pid, Node)
                    end,
                    Connections).


-spec wait_bridge_response() -> {ok, map()} | {error, ?ROUTER_ERROR_CLASSES}.
wait_bridge_response() ->
    receive
        {?SERVER, Response} ->
            Response;
        X ->  %% This resets the wait, it shouldn't be a problem
            io:fwrite("[~p] Unexpected message: ~p~n", [?MODULE, X]),
            wait_bridge_response()
    after ?MAX_WAIT_TIME ->
            io:fwrite("[~p:~p] Wait failed after ~pms~n", [?MODULE, ?LINE, ?MAX_WAIT_TIME]),
            {error, no_response}
    end.
