%%%-------------------------------------------------------------------
%% @doc automate_channel_engine public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine).
-behaviour(gen_server).

%% Public API
-export([create_channel/0, listen_channel/2, send_to_channel/2]).

%% GenServer API
-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-ifdef(TEST).
-export([ping/0]).
-endif.

%%====================================================================
%% API
%%====================================================================
-spec create_channel() -> {ok, binary()} | {error, term(), string()}.
create_channel() ->
    ChannelId = generate_id(),
    case automate_channel_engine_mnesia_backend:register_channel(ChannelId) of
        ok ->
            {ok, ChannelId};
        X ->
            X
    end.

-spec listen_channel(binary(), pid()) -> ok | {error, channel_not_found}.
listen_channel(ChannelId, Pid) ->
    case automate_channel_engine_mnesia_backend:add_listener_to_channel(ChannelId, Pid) of
        ok ->
            ok = gen_server:call(?MODULE, {add_client, Pid});
        X ->
            X
    end.

-spec send_to_channel(binary(), any()) -> ok.
send_to_channel(ChannelId, Message) ->
    {ok, Listeners} = automate_channel_engine_mnesia_backend:get_listeners_on_channel(ChannelId),
    [Pid ! {channel_engine, ChannelId, Message}  || Pid <- Listeners],
    ok.

%%====================================================================
%% GenServer API
%%====================================================================
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call({add_client, Pid}, _From, State) ->
    log("[Channel engine] Linked to ~p~n", [Pid]),
    link(Pid),
    {reply, ok, State};

handle_call(ping, From, State) ->
    log("[Channel engine] Ping received from ~p~n", [From]),
    {reply, pong, State};

handle_call(Msg, From, State) ->
    log("[Channel engine] Call received: ~p from ~p~n", [Msg, From]),
    {noreply, State}.

handle_cast(Msg, State) ->
    log("[Channel engine] Cast received: ~p~n", [Msg]),
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    log("[Channel engine] ~p closed. Reason: ~p~n", [Pid, Reason]),
    automate_channel_engine_mnesia_backend:unlink_listener(Pid),
    {noreply, State};

handle_info(Msg, State) ->
    log("[Channel engine] Info received: ~p~n", [Msg]),
    {noreply, State}.

%%====================================================================
%% Testing API
%%====================================================================

-ifdef(TEST).
ping() ->
    pong = gen_server:call(?MODULE, ping).
-endif.

%%====================================================================
%% Internal functions
%%====================================================================

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

%% @TODO Replace by a real log call
log(Message, Args) ->
    ok.
