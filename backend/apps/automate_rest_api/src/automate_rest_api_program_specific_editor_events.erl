%%% @doc
%%% WebSocket endpoint to listen to updates on a program.
%%% @end

-module(automate_rest_api_program_specific_editor_events).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


-define(FORMATTING, automate_rest_api_utils_formatting).
-define(PING_INTERVAL_MILLISECONDS, 15000).

-include("../../automate_storage/src/records.hrl").

-record(state, { user_id    :: binary()
               , program_id :: binary()
               , error :: none | binary()
               , channel_id :: none | binary()
               }).


init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramId = cowboy_req:binding(program_id, Req),


    Qs = cowboy_req:parse_qs(Req),
    Error = case proplists:get_value(<<"token">>, Qs, undefined) of
        undefined ->
                     <<"Authorization header not found">>;
        X ->
            case automate_rest_api_backend:is_valid_token_uid(X) of
                {true, UserId} ->
                    none;
                {true, TokenUserId} -> %% Non matching user_id
                    io:fwrite("[WS/Program] Url UID: ~p | Token UID: ~p~n", [UserId, TokenUserId]),
                    <<"Unauthorized to use this resource">>;
                false ->
                    <<"Authorization not correct">>
            end
    end,
    {cowboy_websocket, Req, #state{ program_id=ProgramId
                                  , user_id=UserId
                                  , error=Error
                                  , channel_id=none
                                  }}.

websocket_init(State=#state{ program_id=ProgramId
                           , error=none
                           }) ->

    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),

    io:fwrite("[WS/Program] Listening on program ~p; channel: ~p~n", [ProgramId, ChannelId]),
    ok = automate_channel_engine:listen_channel(ChannelId),
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),

    {ok, State#state{ channel_id=ChannelId }};

websocket_init(State=#state{error=Error}) ->
    io:fwrite("[WS/Program] Closing with error: ~p~n", [Error]),
    { reply
    , { close, binary:list_to_bin(
                 lists:flatten(io_lib:format("Error: ~s", [Error]))) }
    , State
    }.


websocket_handle(pong, State) ->
    {ok, State};
websocket_handle({_Type, Message}, State=#state{channel_id=ChannelId}) ->
    Decoded = jiffy:decode(Message, [return_maps]),
    case Decoded of
        #{ <<"type">> := <<"editor_event">> } ->
            automate_channel_engine:send_to_channel(ChannelId, Decoded#{ from_pid => self() });
        _ ->
            ok
    end,
    {ok, State}.


websocket_info(ping_interval, State) ->
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, Message=#{ <<"type">> := <<"editor_event">> }}, State) ->
    Pid = self(),
    case Message of
        #{ from_pid := Pid } ->  % Ignore if the message came from this websocket
            {ok, State};
        _ ->
            {reply, {text, jiffy:encode(maps:remove(from_pid, Message))}, State}
    end;

websocket_info(_Message, State) ->
    {ok, State}.
