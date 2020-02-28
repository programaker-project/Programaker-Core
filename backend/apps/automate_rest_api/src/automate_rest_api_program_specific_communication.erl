%%% @doc
%%% WebSocket endpoint to listen to updates on a program.
%%% @end

-module(automate_rest_api_program_specific_communication).
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
                                  }}.

websocket_init(State=#state{ program_id=ProgramId
                           , error=none
                           }) ->

    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),

    io:fwrite("[WS/Program] Listening on program ~p; channel: ~p~n", [ProgramId, ChannelId]),
    ok = automate_channel_engine:listen_channel(ChannelId),
    timer:send_after(?PING_INTERVAL_MILLISECONDS, ping_interval),

    {ok, State};

websocket_init(State=#state{error=Error}) ->
    io:fwrite("[WS/Program] Closing with error: ~p~n", [Error]),
    { reply
    , { close, binary:list_to_bin(
                 lists:flatten(io_lib:format("Error: ~s", [Error]))) }
    , State
    }.


websocket_handle(pong, State) ->
    {ok, State};
websocket_handle(Message, State) ->
    io:fwrite("[WS/Program] Unexpected message: ~p~n", [Message]),
    {ok, State}.


websocket_info(ping_interval, State) ->
    timer:send_after(?PING_INTERVAL_MILLISECONDS, ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, Message}, State) ->
    case ?FORMATTING:format_message(Message) of
        {ok, Structured} ->
            {reply, {text, jiffy:encode(Structured)}, State};
        {error, Reason} ->
            io:fwrite("[WS/Program]Error formatting: ~p~n~p~n", [Reason, Message]),
            {ok, State}
    end;

websocket_info(Message, State) ->
    io:fwrite("Got ~p~n", [Message]),
    {ok, State}.
