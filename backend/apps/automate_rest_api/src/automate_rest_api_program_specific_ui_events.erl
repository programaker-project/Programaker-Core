%%% @doc
%%% WebSocket endpoint to listen to updates on a program.
%%% @end

-module(automate_rest_api_program_specific_ui_events).
-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).


-define(FORMATTING, automate_rest_api_utils_formatting).
-define(PING_INTERVAL_MILLISECONDS, 15000).

-include("../../automate_storage/src/records.hrl").

-record(state, { user_id    :: binary() | none
               , program_id :: binary()
               , authenticated   :: boolean()
               , can_edit :: boolean()
               , channel_id :: binary() | none
               }).


init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_websocket, Req, #state{ program_id=ProgramId
                                  , user_id=none
                                  , authenticated=false
                                  , can_edit=false
                                  , channel_id=none
                                  }}.


websocket_handle({text, Msg}, State) ->
    handle_message(Msg, State);

websocket_handle({binary, Msg}, State) ->
    handle_message(Msg, State);

websocket_handle(_Message, State) ->
    {ok, State}.

websocket_info(ping_interval, State) ->
    erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),
    {reply, ping, State};

websocket_info({channel_engine, _ChannelId, Message=#{ <<"key">> := ui_events_show }}, State) ->
    {reply, {text, jiffy:encode(Message)}, State};

websocket_info(Message, State) ->
    io:fwrite("Unexpected UI event: ~p~n", [Message]),
    {ok, State}.

%% Message handling
handle_message(Msg, State=#state{ program_id=ProgramId
                                , authenticated=false
                                }) ->
    Data = jiffy:decode(Msg, [return_maps]),
    Passed = case Data of
                 #{ <<"type">> := <<"AUTHENTICATION">>
                  , <<"value">> := #{ <<"token">> := <<"ANONYMOUS">>
                                    }
                  } -> %% TODO: Check that the program/page is publicly available
                     {true, false};
                 #{ <<"type">> := <<"AUTHENTICATION">>
                  , <<"value">> := #{ <<"token">> := Token
                                    }
                  } ->
                     case automate_rest_api_backend:is_valid_token_uid(Token) of
                         {true, TokenUserId} ->
                             {ok, UserCanView} = automate_storage:is_user_allowed({user, TokenUserId}, ProgramId, read_program),
                             {ok, UserCanEdit} = automate_storage:is_user_allowed({user, TokenUserId}, ProgramId, edit_program),
                             case UserCanView of
                                 true -> {true, UserCanEdit};
                                 false ->
                                     automate_logging:log_api(error, ?MODULE, {not_authorized, TokenUserId}),
                                     {false, unauthorized}
                             end;
                         false ->
                             <<"Authorization not correct">>
                     end;
                 _ ->
                     {false, not_found}
             end,
    case Passed of
        {true, CanEdit} ->
            %% Initialize connection. Listen program and set ping
            {ok, #user_program_entry{ program_channel=ChannelId }} = automate_storage:get_program_from_id(ProgramId),
            ok = automate_channel_engine:listen_channel(ChannelId, {ui_events_show, undefined}),
            erlang:send_after(?PING_INTERVAL_MILLISECONDS, self(), ping_interval),

            {ok, State#state{ authenticated=true, can_edit=CanEdit, channel_id=ChannelId }};
        {false, Reason} ->
            automate_logging:log_api(warning, ?MODULE,
                                     list_to_binary(io_lib:format("UI Authentication error on program_id=~p (~p)",
                                                                                     [ ProgramId, Reason ]))),
            { reply
            , { close
              , case Reason of
                    not_found -> <<"Token not found">>;
                    unauthorized -> <<"Not authorized to connect">>
                end
              }
            , State
            }
    end;

handle_message(Msg, State=#state{ channel_id=ChannelId }) ->
    Data = jiffy:decode(Msg, [return_maps]),
    case Data of
        #{ <<"type">> := <<"ui-event">>
         , <<"value">> := #{ <<"action">> := Action
                           , <<"block_type">> := BlockType
                           , <<"block_id">> := BlockId
                           }
         } ->
            ok = automate_channel_engine:send_to_channel(ChannelId, #{ <<"key">> => ui_events
                                                                     , <<"subkey">> => <<BlockType/binary, ".", BlockId/binary>>
                                                                     , <<"value">> => #{ <<"action">> => Action
                                                                                       }
                                                                     })
    end,
    {ok, State}.
