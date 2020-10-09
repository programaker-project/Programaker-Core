%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_service_ports_specific_communication).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include("../../automate_common_types/src/types.hrl").

-record(state, { owner           :: owner_id()
               , service_port_id :: binary()
               , user_channels   :: #{ owner_id() := any() }
               , authenticated   :: boolean()
               }).

init(Req, _Opts) ->
    ServicePortId = cowboy_req:binding(service_port_id, Req),
    {ok, Owner} = automate_service_port_engine:get_bridge_owner(ServicePortId),
    {cowboy_websocket, Req, #state{ service_port_id=ServicePortId
                                  , owner=Owner
                                  , user_channels=#{}
                                  , authenticated=false
                                  }}.

websocket_init(State=#state{ service_port_id=ServicePortId
                           }) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    handle_bridge_message(Msg, State);

websocket_handle({binary, Msg}, State) ->
    handle_bridge_message(Msg, State);

websocket_handle(_Message, State) ->
    {ok, State}.

websocket_info({automate_service_port_engine_router, _From, { data, MessageId, Message }}, State) ->
    Serialized = jiffy:encode(Message#{ <<"message_id">> => MessageId }),
    {reply, {binary, Serialized}, State};

websocket_info({{ automate_service_port_engine, advice_taken}, MessageId, AdviceTaken}, State) ->
    automate_logging:log_api(debug, ?MODULE, {advice_taken, MessageId, AdviceTaken}),
    Serialized = jiffy:encode(#{ <<"type">> => <<"ADVICE_RESPONSE">>
                               , <<"message_id">> => MessageId
                               , <<"value">> => AdviceTaken
                               }),
    {reply, {binary, Serialized}, State};

websocket_info({{ automate_service_port_engine, request_icon}}, State=#state{ service_port_id=ServicePortId }) ->
    automate_logging:log_api(debug, ?MODULE, {requesting_icon, ServicePortId}),
    Serialized = jiffy:encode(#{ <<"type">> => <<"ICON_REQUEST">>
                               }),
    {reply, {binary, Serialized}, State};

websocket_info({ automate_service_port_engine, new_channel, {_ServicePortId, ChannelId}}, State) ->
    ok = automate_channel_engine:monitor_listeners(ChannelId, self(), node()),
    {ok, State};

websocket_info({ automate_channel_engine, add_listener, {Pid, Key, SubKey}}, State=#state{service_port_id=ServicePortId}) ->
    case automate_bot_engine:get_user_from_pid(Pid) of
        {ok, Owner} ->
            %% TODO: In this instance is probably OK to use a single connection
            %%       as the focus are the values, not the keys of SIGNAL_LISTENERS.
            %% But it can be disambiguated by passing more "properties" on the 'add_listener' message.
            case automate_service_port_engine:internal_user_id_to_connection_id(Owner, ServicePortId) of
                {ok, ConnectionId} ->
                    {UserChannels, NewState} = add_to_user_channels(Owner, {Key, SubKey}, State),
                    Serialized = jiffy:encode(#{ <<"type">> => <<"ADVICE_NOTIFICATION">>
                                               , <<"value">> =>
                                                     #{ <<"SIGNAL_LISTENERS">> =>
                                                            #{
                                                              ConnectionId => fmt_user_data(UserChannels)
                                                             }
                                                      }
                                               }),
                    {reply, {binary, Serialized}, NewState};
                {error, Reason} ->
                    automate_logging:log_api(error, ?MODULE, {error, Reason}),
                    {ok, State}
            end;
        {error, not_found} ->
            {ok, State}
    end;

websocket_info(Message, State) ->
    automate_logging:log_api(warning, ?MODULE, {unexpected_message, Message}),
    {ok, State}.

%% State maintenance
merge_user_data(UserData, ChannelData) ->
    sets:add_element(ChannelData, UserData).

create_user_data(ChannelData) ->
    sets:add_element(ChannelData, sets:new()).

fmt_user_data(UserData) ->
    [ fmt_channel_data(ChannelData) || ChannelData <- sets:to_list(UserData) ].

fmt_channel_data({ undefined, _ }) ->
    <<"__all__">>;
fmt_channel_data({ Key, undefined }) ->
    #{ <<"key">> => Key };
fmt_channel_data({ Key, SubKey }) ->
    #{ <<"key">> => Key
     , <<"subkey">> => SubKey
     }.

add_to_user_channels(Owner, ChannelData, State=#state{user_channels=UserChannels}) ->
    case UserChannels of
        #{ Owner := UserData } ->
            NewUserData = merge_user_data(UserData, ChannelData),
            { NewUserData, State#state{ user_channels=UserChannels#{ Owner => NewUserData } } };
        _ ->
            NewUserData = create_user_data(ChannelData),
            { NewUserData, State#state{ user_channels=UserChannels#{ Owner => NewUserData } } }
    end.


handle_bridge_message(Msg, State=#state{ service_port_id=BridgeId
                                       , authenticated=false
                                       }) ->
    Data = jiffy:decode(Msg, [return_maps]),
    Passed = case Data of
                 #{ <<"type">> := <<"AUTHENTICATION">>
                  , <<"value">> := #{ <<"token">> := Token
                                    }
                  } ->
                     case automate_service_port_engine:check_bridge_token(BridgeId, Token) of
                         {ok, true} -> true;
                         {ok, false} ->
                             {false, mismatch}
                     end;
                 _ ->
                     {ok, Answer} = automate_service_port_engine:can_skip_authentication(BridgeId),
                     case Answer of
                         true -> skip;
                         false -> {false, not_found}
                     end
             end,
    case Passed of
        true ->
            ok = automate_service_port_engine:register_service_port(BridgeId),
            {ok, State#state{ authenticated=true }};
        skip ->
            ok = automate_service_port_engine:register_service_port(BridgeId),
            handle_bridge_message(Msg, State#state{ authenticated=true });
        {false, Reason} ->
            automate_logging:log_api(warning, ?MODULE,
                                     binary:list_to_bin(lists:flatten(io_lib:format("Authentication error on bridge_id=~p (~p)",
                                                                                    [ BridgeId, Reason ])))),
            { reply
            , { close
              , case Reason of
                    mismatch -> <<"Not matching token">>;
                    not_found -> <<"Token not found">>
                end
              }
            , State
            }
    end;

handle_bridge_message(Msg, State=#state{ service_port_id=ServicePortId
                                       , owner=Owner
                                       }) ->
    try automate_service_port_engine:from_service_port(ServicePortId, Owner, Msg) of
        _ ->
            {ok, State}
    catch ErrorNs:Error:StackTrace ->
            automate_logging:log_api(error, ?MODULE, binary:list_to_bin(
                                                       lists:flatten(io_lib:format("~p:~p~n~p", [ErrorNs, Error, StackTrace])))),
            { reply
            , { close
              , binary:list_to_bin(
                  lists:flatten(io_lib:format("~p~p", [ErrorNs, Error])))}
            , State
            }
    end.
