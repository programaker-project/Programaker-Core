-module(automate_service_port_engine_router).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , connect_bridge/1
        , call_bridge/2
        , is_bridge_connected/1
        , answer_message/2
        ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3
        ]).

-define(SERVER, ?MODULE).
-define(MAX_WAIT_TIME_SECONDS, 100).
-define(MAX_WAIT_TIME, ?MAX_WAIT_TIME_SECONDS * 1000).

-record(state, { bridge_by_id
               , bridge_id_by_pid
               , thread_by_message
               }).
-record(bridge_info, { pid
                     , bridge_id
                     }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Connect a bridge to the router.
%%
%% @spec connect_bridge(BridgeId) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
connect_bridge(BridgeId) ->
    gen_server:cast({global, ?SERVER}, { connect_bridge, BridgeId, self() }).

%%--------------------------------------------------------------------
%% @doc
%% Send a call to a bridge and return the result.
%%
%% @spec call_bridge(BridgeId, Msg) -> {ok, Value} | {error, Error}
%% @end
%%--------------------------------------------------------------------
call_bridge(BridgeId, Msg) ->
    gen_server:cast({global, ?SERVER}, { call_bridge, BridgeId, Msg, self() }),
    wait_server_response().

wait_server_response() ->
    receive
        {?SERVER, Response} ->
            Response;
        X ->
            io:fwrite("WTF: ~p~n", [X]),
            wait_server_response()
    after ?MAX_WAIT_TIME ->
            {error, no_response}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Route a message answer.
%%
%% @spec answer_message(MessageId, Response) -> {ok, boolean} | {error, Error}
%% @end
%%--------------------------------------------------------------------
answer_message(MessageId, Response) ->
    gen_server:call({global, ?SERVER}, { answer_message, MessageId, Response }).

%%--------------------------------------------------------------------
%% @doc
%% Check if a bridge has server connections.
%%
%% @spec is_bridge_connected(BridgeId) -> {ok, boolean} | {error, Error}
%% @end
%%--------------------------------------------------------------------
is_bridge_connected(BridgeId) ->
    gen_server:call({global, ?SERVER}, { is_bridge_connected, BridgeId }).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{ bridge_by_id=#{}
               , bridge_id_by_pid=#{}
               , thread_by_message=#{}
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({ is_bridge_connected, BridgeId }, _From, State) ->
    io:fwrite("Is ~p connected on ~p?~n", [BridgeId, State]),
    IsConnected = case State of
                      #state{ bridge_by_id=#{ BridgeId := Connections }} ->
                          length(Connections) > 0;
                      _ ->
                          false
                  end,

    {reply, {ok, IsConnected}, State};

handle_call({ answer_message, MessageId, Result }, _From, State) ->
    case State of
        #state{ thread_by_message=Messages=#{ MessageId := ThreadId } } ->
            io:fwrite("Sending message to ~p~n", [ThreadId]),
            ThreadId ! { ?SERVER, {ok, Result }},
            {reply, ok, State#state{ thread_by_message=maps:remove(MessageId, Messages)
                                   }};
        _ ->
            io:fwrite("MessageId(~p) not found on state(~p)~n", [MessageId, State]),
            {reply, { error, no_message_id  }, State}
    end;

handle_call({ get_routes }, _From, State) ->
    #state{ bridge_by_id=Routes } = State,
    Reply = {ok, Routes},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({ call_bridge, BridgeId, Msg, From  }, State) ->
    case State of
        #state{ bridge_by_id=Bridges=#{ BridgeId := Connections }
              , thread_by_message=Messages
              } ->
            AliveConnections = check_alive_connections(Connections),
            case AliveConnections of
                [First | Rest] ->
                    {MessageId, ThreadId, WithMessaging} = call_bridge_to_connection(First, Msg, From),

                    { noreply
                    , State#state { bridge_by_id=Bridges#{ BridgeId => Rest ++ [WithMessaging] }
                                  , thread_by_message=Messages#{ MessageId => ThreadId
                                                               }
                                  }
                    };
                _ ->
                    io:fwrite("No alive connections left on bridgeId (~p)~n", [BridgeId]),
                    From ! { ?SERVER, { error, no_connection }},
                    {noreply, State}
            end;
        _ ->
            io:fwrite("BridgeId (~p) in state(~p)~n", [BridgeId, State]),
            From ! { ?SERVER, { error, no_connection }},
            {noreply, State}
    end;

handle_cast({ connect_bridge, BridgeId, Pid }, State) ->
    #state{ bridge_by_id=Bridges
          , bridge_id_by_pid=BridgesByPid
          } = State,
    PreexistingBridges = case maps:find(BridgeId, Bridges) of
                              { ok, OtherBridges } -> OtherBridges;
                              error -> []
                          end,
    link(Pid),
    {noreply, State#state{
                bridge_by_id=maps:put(BridgeId,
                                  [ #bridge_info{pid=Pid, bridge_id=BridgeId}
                                    | PreexistingBridges
                                  ], Bridges),
               bridge_id_by_pid=maps:put(Pid, BridgeId, BridgesByPid)
               }
    };

handle_cast(_Msg, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a message to all bridges in a list.
%%
%% @spec send_to_all(msg, bridges) -> {ok, ContinuingBridges} | { error, Error }
%% @end
%%--------------------------------------------------------------------
-spec call_bridge_to_connection(#bridge_info{}, any(), pid()) -> #bridge_info{}.
call_bridge_to_connection(Bridge=#bridge_info{ pid=Pid}, Msg, From) ->
    MessageId = generate_id(),
    Pid ! { ?SERVER, Pid, { data, MessageId, Msg }},
    {MessageId, From, Bridge}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State=#state{ bridge_by_id=Bridges, bridge_id_by_pid=BridgesByPid}) ->
    %% TODO: consider calling threads failing
    {NewBridges, NewBridgesById} =
        case BridgesByPid of
            #{ Pid := BridgeId } ->
                WithoutBridgePid = maps:remove(Pid, BridgesByPid),

                case Bridges of
                    #{ BridgeId := BridgeData } ->
                        FilteredBridgeData = lists:filter(fun (#bridge_info{pid=InstancePid}) ->
                                                                  Pid == InstancePid
                                                          end, BridgeData),
                        case length(FilteredBridgeData) of
                            0 ->
                                {maps:remove(BridgeId, Bridges), WithoutBridgePid};
                            _ ->
                                {maps:update(BridgeId, FilteredBridgeData, Bridges), WithoutBridgePid}
                        end;
                    _ ->
                        io:fwrite("[01] Not found ~p on ~p~n", [BridgeId, Bridges]),
                        { Bridges, WithoutBridgePid}
                end;
            _ ->
                io:fwrite("[02] Not found ~p on ~p~n", [Pid, BridgesByPid]),
                {Bridges, BridgesByPid}
        end,

    {noreply, State#state{ bridge_by_id=NewBridges
                         , bridge_id_by_pid=NewBridgesById
                         }};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

check_alive_connections(Connections) ->
    lists:filter(fun(#bridge_info{ pid=Pid }) ->
                         erlang:is_process_alive(Pid) end,
                 Connections).
