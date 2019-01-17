-module(automate_service_port_engine_router).

-behaviour(gen_server).
-define(TIME_BETWEEN_REPORTS_IN_SECONDS, 5).
-define(TIME_BETWEEN_REPORTS, ?TIME_BETWEEN_REPORTS_IN_SECONDS * 1000).

%% API
-export([start_link/0
        , route_inbound/2
        , open_outbound_channel/2
        , get_routes/0
        ]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(stats, {processed_messages}).
-record(state, {channels, stats}).
-record(trigger, {pid, callback}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Obtain available routes.
%%
%% @spec get_routes() -> {ok, Routes} | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_routes() ->
    gen_server:call({global, ?SERVER}, { get_routes }).

%%--------------------------------------------------------------------
%% @doc
%% Register an outbound channel.
%%
%% @spec open_outbound_channel(ChannelId, Callback) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
open_outbound_channel(ChannelId, Callback) ->
    gen_server:cast({global, ?SERVER}, { open_outbound_channel, ChannelId, self(), Callback }).

%%--------------------------------------------------------------------
%% @doc
%% Send a message through a inbound channel.
%%
%% @spec route_inbound(ChannelId, Msg) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
route_inbound(ChannelId, Msg) ->
    gen_server:call({global, ?SERVER}, { route_inbound, ChannelId, Msg }).

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
    erlang:send_after(?TIME_BETWEEN_REPORTS, ?SERVER, {send_reports}),
    {ok, #state{ channels=#{}
               , stats=#stats{ processed_messages=0
                             }
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
handle_call({ get_routes }, _From, State) ->
    #state{ channels=Routes } = State,
    Reply = {ok, Routes},
    {reply, Reply, State};

handle_call({ route_inbound, ChannelId, Msg }, _From, State) ->
    #state{ channels=Channels } = State,
    #state{ stats=Stats} = State,
    #stats{ processed_messages=ProcessedMessages } = Stats,

    Response = case maps:find(ChannelId, Channels) of
        error -> [];
        { ok, OutboundChannels } -> OutboundChannels
    end,

    io:fwrite("Channels: ~p~n", [Channels]),
    { reply
    , length(Response)
    , State#state { channels=Channels#{ ChannelId => send_to_all(Msg, Response) }
                  , stats=Stats#stats{ processed_messages=(ProcessedMessages + 1)
                                     }
                  }};

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
handle_cast({ open_outbound_channel, ChannelId, Pid, Callback }, State) ->
    #state{ channels=Channels } = State,
    PreexistingChannels = case maps:find(ChannelId, Channels) of
                              { ok, OtherChannels } -> OtherChannels;
                              error -> []
                          end,
    {noreply, State#state{
                channels=maps:put(ChannelId,
                                  [ #trigger{pid=Pid, callback=Callback}
                                    | PreexistingChannels
                                  ], Channels)
               }
    };

handle_cast(_Msg, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a message to all channels in a list.
%%
%% @spec send_to_all(msg, channels) -> {ok, ContinuingChannels} | { error, Error }
%% @end
%%--------------------------------------------------------------------
-spec send_to_all(any(), [ #trigger{} ]) -> [#trigger{}] | {error, any()}.
send_to_all(Msg, Channels) ->
    io:fwrite("Sending to ~p~n", [Channels]),
    send_to_all(Msg, Channels, []).

send_to_all(_, [], Continuing) ->
    Continuing;

send_to_all(Msg, [ Channel=#trigger{ pid=_Pid, callback=Callback } | T], Continuing) ->
    case Callback(Msg) of
        continue ->
            send_to_all(Msg, T, [ Channel | Continuing ]);
        _ ->
            send_to_all(Msg, T, Continuing)
    end.

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
handle_info({send_reports}, #state{ channels=Channels, stats=Stats}) ->
    #stats{processed_messages=ProcessedMessages} = Stats,
    monitor:add_processed_messages(ProcessedMessages),
    erlang:send_after(?TIME_BETWEEN_REPORTS, ?SERVER, {send_reports}),
    {noreply, #state{ channels=Channels
                    , stats=#stats{ processed_messages=0
                            }
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
