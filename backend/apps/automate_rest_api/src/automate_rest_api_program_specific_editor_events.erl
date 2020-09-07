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

    {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),

    automate_logging:log_api(debug, ?MODULE,
                             io_lib:format("Listening on program ~p; channel: ~p~n", [ProgramId, ChannelId])),
    ok = automate_channel_engine:monitor_listeners(ChannelId, self(), node()),
    ok = automate_channel_engine:listen_channel(ChannelId),

    Events = case automate_storage:get_program_events(ProgramId) of
                 {ok, Evs} ->
                     lists:map(fun(#user_program_editor_event{ event=Ev }) ->
                                       {text, jiffy:encode(Ev)}
                               end, Evs);
                 _ ->
                     []
             end,

    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),

    EndMarker = jiffy:encode(#{ <<"type">> => <<"editor_event">>
                              , <<"value">> => #{ <<"type">> => <<"ready">>
                                                , <<"value">> => #{}
                                                }
                              }),

    {reply, Events ++ [{text, EndMarker}], State#state{ channel_id=ChannelId }};

websocket_init(State=#state{error=Error}) ->
    automate_logging:log_api(warning, ?MODULE,
                             io_lib:format("Closing with error: ~p~n", [Error])),
    { reply
    , { close, binary:list_to_bin(
                 lists:flatten(io_lib:format("Error: ~s", [Error]))) }
    , State
    }.


websocket_handle(pong, State) ->
    {ok, State};
websocket_handle(_, State=#state{can_edit=false}) ->
    {reply
    , { close, <<"Not authorized to send events">> }
    , State
    };
websocket_handle({_Type, Message}, State=#state{program_id=ProgramId, channel_id=ChannelId}) ->
    Decoded = jiffy:decode(Message, [return_maps]),
    case Decoded of
        #{ <<"type">> := <<"editor_event">> } ->
            ok = automate_channel_engine:send_to_channel(ChannelId, Decoded#{ from_id => self() }),
            ok = case Decoded of
                     #{ <<"value">> := #{ <<"save">> := true } } ->
                         automate_storage:store_program_event(ProgramId, Decoded);
                     _ ->
                         ok
                 end;
        _ ->
            ok
    end,
    {ok, State}.

websocket_info({ automate_channel_engine, add_listener, {Pid, _Key, _SubKey}}, State) ->
    Self = self(),
    case Pid of
        Self ->
            {ok, State};
        _ ->
            {reply, {text, jiffy:encode(#{ <<"type">> => <<"editor_event">>
                                         , <<"value">> => #{ <<"type">> => <<"add_editor">>
                                                           , <<"value">> => #{  <<"id">> => list_to_binary(pid_to_list(Pid)) }
                                                           }
                                         })}, State}
    end;

websocket_info({automate_channel_engine,remove_listener, {Pid, _Channel}}, State) ->
    Self = self(),
    case Pid of
        Self ->
            {ok, State};
        _ ->
            {reply, {text, jiffy:encode(#{ <<"type">> => <<"editor_event">>
                                         , <<"value">> => #{ <<"type">> =>  <<"remove_editor">>
                                                           , <<"value">> => #{  <<"id">> => list_to_binary(pid_to_list(Pid)) }
                                                           }
                                         })}, State}
    end;

websocket_info(ping_interval, State) ->
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, Message=#{ <<"type">> := <<"editor_event">> }}, State) ->
    Pid = self(),
    case Message of
        #{ from_id := Pid } ->  % Ignore if the message came from this websocket
            {ok, State};
        _ ->
            NewMessage = case Message of
                             #{ from_id := NewPid, <<"value">> := Value=#{ <<"value">> := InnerValue }} ->
                                 Clean = maps:remove(from_id, Message),
                                 Clean#{ <<"value">> => Value#{ <<"value">> => InnerValue#{ <<"id">> => list_to_binary(pid_to_list(NewPid)) }}};
                             _ ->
                                 Message
                     end,
            {reply, {text, jiffy:encode(NewMessage)}, State}
    end;

websocket_info(_Message, State) ->
    {ok, State}.
