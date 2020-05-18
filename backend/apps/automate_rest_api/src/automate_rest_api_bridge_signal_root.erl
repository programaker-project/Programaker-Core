%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_bridge_signal_root).
-export([ init/2 ]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(PING_INTERVAL_MILLISECONDS, 15000).

-record(state, { user_id    :: binary()
               , bridge_id  :: binary()
               , authorized :: boolean()
               , errorCode  :: binary() | none
               }).

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    BridgeId = cowboy_req:binding(bridge_id, Req),
    {IsAuthorized, ErrorCode} = check_is_authorized(Req, UserId),

    {cowboy_websocket, Req, #state{ bridge_id=BridgeId
                                  , user_id=UserId
                                  , authorized=IsAuthorized
                                  , errorCode=ErrorCode
                                  }}.


check_is_authorized(Req, UserId) ->
    case cowboy_req:header(<<"authorization">>, Req, undefined) of
        undefined ->
            { false, <<"Authorization header not found">> };
        X ->
            case automate_rest_api_backend:is_valid_token_uid(X) of
                {true, UserId} ->
                    { true, none };
                {true, TokenUserId} -> %% Non matching user_id
                    io:fwrite("Url UID: ~p | Token UID: ~p~n", [UserId, TokenUserId]),
                    { false, <<"Unauthorized to connect here">> };
                false ->
                    { false, <<"Authorization not correct">> }
            end
    end.

websocket_init(State=#state{ bridge_id=BridgeId
                           , user_id=UserId
                           , authorized=IsAuthorized
                           , errorCode=ErrorCode
                           }) ->
    case IsAuthorized of
        false ->
            { reply, { close, ErrorCode }, State };
        true ->
            case automate_service_port_engine:listen_bridge(BridgeId, UserId) of
                ok ->
                    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
                    {ok, State};
                {error, Error} ->
                    { reply, { close, io_lib:format("Error: ~p", [Error]) }, State }
            end
        end.

websocket_handle(_Message, State) ->
    %% Ignore everything
    {ok, State}.


websocket_info(ping_interval, State) ->
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _From,  Data }, State) ->
    Serialized = jiffy:encode(Data),
    {reply, {binary, Serialized}, State};

websocket_info(Message, State) ->
    io:fwrite("Unexpected message: ~p~n", [Message]),
    {reply, {binary, Message}, State}.
