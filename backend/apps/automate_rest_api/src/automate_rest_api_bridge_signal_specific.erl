%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_bridge_signal_specific).
-export([ init/2 ]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(PING_INTERVAL_MILLISECONDS, 15000).

-record(state, { user_id    :: binary()
               , bridge_id  :: binary()
               , key        :: binary()
               , authorized :: boolean()
               , errorCode  :: binary() | none
               }).

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Key = cowboy_req:binding(key, Req),
    {IsAuthorized, ErrorCode} = check_is_authorized(Req, UserId),

    {cowboy_websocket, Req, #state{ user_id=UserId
                                  , bridge_id=BridgeId
                                  , key=Key
                                  , authorized=IsAuthorized
                                  , errorCode=ErrorCode
                                  }}.


check_is_authorized(Req, UserId) ->
    case cowboy_req:header(<<"authorization">>, Req, undefined) of
        undefined ->
            { false, <<"Authorization header not found">> } ;
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
                           , key=Key
                           , authorized=IsAuthorized
                           , errorCode=ErrorCode
                           }) ->
    case IsAuthorized of
        false ->
            { reply, { close, ErrorCode }, State };
        true ->
            case automate_service_port_engine:listen_bridge(BridgeId, {user, UserId}, {Key}) of
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

websocket_info({channel_engine, _From,  Data=#{ <<"key">> := Key } },
               State=#state{ key=Key }) ->
    Serialized = jiffy:encode(Data),
    {reply, {binary, Serialized}, State};

websocket_info({channel_engine, _From,  #{ <<"key">> := _AnotherKey } },
               State=#state{ key=_Key }) ->
    %% TODO: This can be used to test that only relevant data is sent
    {ok, State};

websocket_info(Message, State) ->
    io:fwrite("Unexpected message: ~p~n", [Message]),
    {reply, {binary, Message}, State}.
