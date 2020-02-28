%%% @doc
%%% WebSocket endpoint to listen to completion on a pending connection.
%%% @end

-module(automate_rest_api_connections_pending_wait).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


-define(PING_INTERVAL_MILLISECONDS, 15000).
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { user_id    :: binary()
               , connection_id :: binary()
               }).


init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ConnectionId = cowboy_req:binding(connection_id, Req),

    {cowboy_websocket, Req, #state{ connection_id=ConnectionId
                                  , user_id=UserId
                                  }}.

websocket_init(State=#state{ connection_id=ConnectionId
                           }) ->

    {ok, #user_to_bridge_pending_connection_entry{ channel_id=ChannelId }} = automate_service_port_engine:get_pending_connection_info(ConnectionId),

    io:fwrite("[WS/Pending Connection] Listening on connection ~p; channel: ~p~n", [ConnectionId, ChannelId]),
    ok = automate_channel_engine:listen_channel(ChannelId),
    timer:send_after(?PING_INTERVAL_MILLISECONDS, ping_interval),

    {ok, State}.

websocket_handle(pong, State) ->
    {ok, State};
websocket_handle(Message, State) ->
    io:fwrite("[WS/Pending Connection] Unexpected message: ~p~n", [Message]),
    {ok, State}.


websocket_info(ping_interval, State) ->
    timer:send_after(?PING_INTERVAL_MILLISECONDS, ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, connection_established}, State) ->
    {reply, [ { text, jiffy:encode(#{ success => true, type => connection_established }) }
            , { close, 1000, <<"Wait completed">> }
            ], State};

websocket_info(Message, State) ->
    io:fwrite("Got ~p~n", [Message]),
    {ok, State}.