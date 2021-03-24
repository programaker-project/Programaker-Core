%%% @doc
%%% WebSocket endpoint to listen to updates on a program.
%%% @end

-module(automate_rest_api_program_specific_variables_stream).
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
    ProgramId = cowboy_req:binding(program_id, Req),

    Qs = cowboy_req:parse_qs(Req),
    {Error, UserId} = case proplists:get_value(<<"token">>, Qs, undefined) of
        undefined ->
                     {<<"Authorization header not found">>, none};
        X ->
            case automate_rest_api_backend:is_valid_token_uid(X, {read_program_variables, ProgramId}) of
                {true, TokenUserId} ->
                    case automate_storage:is_user_allowed({user, TokenUserId}, ProgramId, edit_program) of
                        {ok, true} -> {none, TokenUserId};
                        {ok, false} -> automate_logging:log_api(warning, ?MODULE,
                                             io_lib:format("[WS/Program] Token UID: ~p~n", [TokenUserId])),
                                       {<<"Unauthorized to use this resource">>, none}
                    end;
                false ->
                    {<<"Authorization not correct">>, none}
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

    ok = automate_channel_engine:listen_channel(ChannelId, { variable_events, undefined }),

    automate_logging:log_api(info, ?MODULE,
                             binary:list_to_bin(io_lib:format("[WS/Program/Variables] Listening on program ~p; channel: ~p~n", [ProgramId, ChannelId]))),
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),

    {ok, State};

websocket_init(State=#state{error=Error}) ->
    automate_logging:log_api(warning, ?MODULE, {"[WS/Program/Variables] Closing with error", Error}),
    { reply
    , { close, binary:list_to_bin(
                 lists:flatten(io_lib:format("Error: ~s", [Error]))) }
    , State
    }.

websocket_handle(ping, State) ->
    {ok, State};
websocket_handle({ping, _}, State) ->
    {ok, State};
websocket_handle(pong, State) ->
    {ok, State};
websocket_handle({pong, _}, State) ->
    {ok, State};
websocket_handle(Message, State) ->
    automate_logging:log_api(warning, ?MODULE, {unexpected_message, Message}),
    {ok, State}.


websocket_info(ping_interval, State) ->
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, #{ <<"key">> := variable_events
                                             , <<"subkey">> := ReceivedVariable
                                             , <<"value">> := Value
                                             }}, State)  ->
    Update = #{ name => ReceivedVariable, value => Value },
    try jiffy:encode(Update) of
        Encoded when is_binary(Encoded) ->
            {reply, {text, Encoded}, State}
    catch ErrorNS:Error:ST ->
            automate_logging:log_platform(error, ErrorNS, Error, ST),
            {reply, {text, jiffy:encode(#{ name => ReceivedVariable, value => unknown })}, State}
    end;

websocket_info(_Message, State) ->
     %% io:fwrite("[D: ???]"),
    {ok, State}.
