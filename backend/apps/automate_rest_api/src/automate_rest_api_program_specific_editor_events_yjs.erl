%%% @doc
%%% WebSocket endpoint to listen to updates on a program using Yjs's protocol.
%%% @end

-module(automate_rest_api_program_specific_editor_events_yjs).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


-define(FORMATTING, automate_rest_api_utils_formatting).
-define(PING_INTERVAL_MILLISECONDS, 15000).

-include("../../automate_storage/src/records.hrl").

-record(state, { user_id    :: binary() | none
               , program_id :: binary()
               , error :: none | binary()
               , channel_id :: none | binary()
               , can_edit :: boolean()
               }).


init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),

    Qs = cowboy_req:parse_qs(Req),
    {Error, UserId, CanEdit} = case proplists:get_value(<<"token">>, Qs, undefined) of
        undefined ->
                     {<<"Authorization header not found">>, none, false};
        X ->
            case automate_rest_api_backend:is_valid_token_uid(X) of
                {true, TokenUserId} ->
                    {ok, UserCanView} = automate_storage:is_user_allowed({user, TokenUserId}, ProgramId, read_program),
                    {ok, UserCanEdit} = automate_storage:is_user_allowed({user, TokenUserId}, ProgramId, edit_program),
                    case UserCanView of
                        true -> {none, TokenUserId, UserCanEdit};
                        false ->
                            automate_logging:log_api(error, ?MODULE, {not_authorized, TokenUserId}),
                            {<<"Unauthorized to use this resource">>, TokenUserId, false}
                    end;
                false ->
                    <<"Authorization not correct">>
            end
    end,
    {cowboy_websocket, Req, #state{ program_id=ProgramId
                                  , user_id=UserId
                                  , error=Error
                                  , channel_id=none
                                  , can_edit=CanEdit
                                  }}.

websocket_init(State=#state{ program_id=ProgramId
                           , error=none
                           }) ->

    {ok, #user_program_entry{ program_channel=ProgramChannelId }} = automate_storage:get_program_from_id(ProgramId),

    automate_logging:log_api(debug, ?MODULE,
                             io_lib:format("Listening on program ~p; channel: ~p~n", [ProgramId, ProgramChannelId])),
    {ok, ChannelId} = case automate_channel_engine:listen_channel(ProgramChannelId) of
                          ok -> {ok, ProgramChannelId};
                          {error, channel_not_found} ->
                              automate_logging:log_api(warning, ?MODULE, {fixing, program_channel, ProgramId}),
                              ok = automate_storage:fix_program_channel(ProgramId),
                              {ok, #user_program_entry{ program_channel=NewChannelId }} = automate_storage:get_program_from_id(ProgramId),
                              {automate_channel_engine:listen_channel(NewChannelId), NewChannelId}
                      end,

    Events = case automate_storage:get_program_events(ProgramId) of
                 {ok, Evs} ->
                     lists:map(fun(#user_program_editor_event{ event=Ev }) ->
                                       {binary, Ev}
                               end, Evs);
                 _ ->
                     []
             end,

    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),


    {reply, Events, State#state{ channel_id=ChannelId }};

websocket_init(State=#state{error=Error}) ->
    automate_logging:log_api(warning, ?MODULE,
                             io_lib:format("Closing with error: ~p~n", [Error])),
    { reply
    , { close, binary:list_to_bin(
                 lists:flatten(io_lib:format("Error: ~s", [Error]))) }
    , State
    }.

websocket_handle({Type, _}, State=#state{can_edit=false}) when (Type == text) orelse (Type == binary) ->
    {reply
    , { close, <<"Not authorized to send events">> }
    , State
    };
websocket_handle({binary, Message}, State=#state{program_id=ProgramId, channel_id=ChannelId}) ->

    {Type, Contents} = case Message of
                           <<0:8, C/binary>> ->
                               automate_storage:store_program_event(ProgramId, Message),
                               {sync, C};
                           <<1:8, C/binary>> ->
                               {awareness, C}
                       end,
    ok = automate_channel_engine:send_to_channel(ChannelId, #{ from_id => self(), type => yjs_event, data => {Type, Contents} }),

    {ok, State};
websocket_handle(_, State) ->
    {ok, State}.

websocket_info(ping_interval, State) ->
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, Message=#{ type := yjs_event
                                                     }}, State) ->
    Pid = self(),
    case Message of
        #{ from_id := Pid } ->  % Ignore if the message came from this websocket
            {ok, State};
        _ ->
            case Message of
                #{ data := {sync, Msg} } ->
                    {reply, {binary, <<0:8, Msg/binary>>}, State};
                #{ data := {awareness, _Msg} } ->
                    {ok, State};
                _ ->
                    io:fwrite("‽"),
                    {ok, State}
            end
    end;

websocket_info(_Message, State) ->
    {ok, State}.
