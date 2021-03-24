%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_bridge_signal_root).
-export([ init/2 ]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(PING_INTERVAL_MILLISECONDS, 15000).
-include("../../automate_storage/src/records.hrl").

-record(state, { owner :: owner_id()
               , bridge_id  :: binary()
               , authorized :: boolean()
               , errorCode  :: binary() | none
               }).

init(Req, _Opts) ->
    BridgeId = cowboy_req:binding(bridge_id, Req),
    {IsAuthorized, ErrorCode, Owner} = check_is_authorized(Req, BridgeId),

    {cowboy_websocket, Req, #state{ bridge_id=BridgeId
                                  , owner=Owner
                                  , authorized=IsAuthorized
                                  , errorCode=ErrorCode
                                  }}.


check_is_authorized(Req, BridgeId) ->
    Qs = cowboy_req:parse_qs(Req),
    Token = case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    proplists:get_value(<<"token">>, Qs, undefined);
                Tok -> Tok
            end,


    case Token of
        undefined ->
            { false, <<"Authorization data not found">>, undefined };
        X ->
            case automate_rest_api_backend:is_valid_token_uid(X, { read_bridge_signal, BridgeId }) of
                {true, UserId} ->
                    case proplists:get_value(<<"as_group">>, Qs, undefined) of
                        undefined ->
                            { true, none, {user, UserId} };
                        GroupId ->
                            case automate_storage:can_user_edit_as({user, UserId}, {group, GroupId}) of
                                true ->
                                    { true, none, {group, GroupId} };
                                false ->
                                    { false, <<"Unauthorized operation">>, undefined }
                            end
                    end;
                false ->
                    { false, <<"Authorization not correct">>, undefined }
            end
    end.

websocket_init(State=#state{ bridge_id=BridgeId
                           , owner=Owner
                           , authorized=IsAuthorized
                           , errorCode=ErrorCode
                           }) ->
    case IsAuthorized of
        false ->
            { reply, { close, ErrorCode }, State };
        true ->
            case automate_service_port_engine:listen_bridge(BridgeId, Owner) of
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
    automate_logging:log_api(warning, ?MODULE, {unexpected_message, Message}),
    {reply, {binary, Message}, State}.
