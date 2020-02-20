%%% @doc
%%% WebSocket endpoint to listen to updates on a program.
%%% @end

-module(automate_rest_api_program_specific_communication).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


-define(FORMATTING, automate_rest_api_utils_formatting).
-include("../../automate_storage/src/records.hrl").

-record(state, { user_id    :: binary()
               , program_id :: binary()
               , program_channel :: binary()
               }).

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramId = cowboy_req:binding(program_id, Req),
    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),

    {cowboy_websocket, Req, #state{ program_id=ProgramId
                                  , user_id=UserId
                                  , program_channel=ChannelId
                                  }}.

websocket_init(State=#state{ program_id=ProgramId
                           , program_channel=ChannelId
                           }) ->
    io:fwrite("[WS/Program] Listening on program ~p; channel: ~p~n", [ProgramId, ChannelId]),
    ok = automate_channel_engine:listen_channel(ChannelId),

    {ok, State}.

websocket_handle(Message, State) ->
    io:fwrite("[WS/Program] Unexpected message: ~p~n", [Message]),
    {ok, State}.

websocket_info({channel_engine, _ProgramId, Message}, State) ->
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
