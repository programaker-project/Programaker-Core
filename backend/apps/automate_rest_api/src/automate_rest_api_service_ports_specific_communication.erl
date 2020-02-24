%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_service_ports_specific_communication).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, { user_id         :: binary()
               , service_port_id :: binary()
               , user_channels   :: #{ binary() := any() }
               }).

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ServicePortId = cowboy_req:binding(service_port_id, Req),

    {cowboy_websocket, Req, #state{ service_port_id=ServicePortId
                                  , user_id=UserId
                                  , user_channels=#{}
                                  }}.

websocket_init(State=#state{ service_port_id=ServicePortId
                           }) ->
    automate_service_port_engine:register_service_port(ServicePortId),
    {ok, State}.

websocket_handle({text, Msg}, State=#state{ service_port_id=ServicePortId
                                          , user_id=UserId
                                          }) ->
    automate_service_port_engine:from_service_port(ServicePortId, UserId, Msg),
    {ok, State};

websocket_handle({binary, Msg}, State=#state{ service_port_id=ServicePortId
                                            , user_id=UserId
                                            }) ->
    automate_service_port_engine:from_service_port(ServicePortId, UserId, Msg),
    {ok, State};

websocket_handle(_Message, State) ->
    {ok, State}.

websocket_info({automate_service_port_engine_router, _From, { data, MessageId, Message }}, State) ->
    io:fwrite("[~p] New message: ~p~n", [MessageId, Message]),
    Serialized = jiffy:encode(Message#{ <<"message_id">> => MessageId }),
    {reply, {binary, Serialized}, State};

websocket_info({{ automate_service_port_engine, advice_taken}, MessageId, AdviceTaken}, State) ->
    io:fwrite("[~p] Advice taken: ~p~n", [MessageId, AdviceTaken]),
    Serialized = jiffy:encode(#{ <<"type">> => <<"ADVICE_RESPONSE">>
                               , <<"message_id">> => MessageId
                               , <<"value">> => AdviceTaken
                               }),
    {reply, {binary, Serialized}, State};

websocket_info({ automate_service_port_engine, new_channel, {_ServicePortId, ChannelId}}, State) ->
    ok = automate_channel_engine:monitor_listeners(ChannelId, self(), node()),
    {ok, State};

websocket_info({ automate_channel_engine, add_listener, {Pid, Key, SubKey}}, State=#state{service_port_id=ServicePortId}) ->
    case automate_bot_engine:get_user_from_pid(Pid) of
        {ok, UserId} ->
            %% TODO: In this instance is probably OK to use a single connection.
            %%   But it can be disambiguated by passing more "properties" on the 'add_listener' message.
            {ok, ConnectionId} = automate_service_port_engine:internal_user_id_to_connection_id(UserId, ServicePortId),
            {UserChannels, NewState} = add_to_user_channels(UserId, {Key, SubKey}, State),
            Serialized = jiffy:encode(#{ <<"type">> => <<"ADVICE_NOTIFICATION">>
                                       , <<"value">> =>
                                             #{ <<"SIGNAL_LISTENERS">> =>
                                                    #{
                                                      ConnectionId => fmt_user_data(UserChannels)
                                                     }
                                              }
                                       }),
            {reply, {binary, Serialized}, NewState};
        {error, not_found} ->
            {ok, State}
    end;

websocket_info(Message, State) ->
    io:fwrite("Got ~p~n", [Message]),
    {reply, {binary, Message}, State}.


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

add_to_user_channels(UserId, ChannelData, State=#state{user_channels=UserChannels}) ->
    case UserChannels of
        #{ UserId := UserData } ->
            NewUserData = merge_user_data(UserData, ChannelData),
            { NewUserData, State#state{ user_channels=UserChannels#{ UserId => NewUserData } } };
        _ ->
            NewUserData = create_user_data(ChannelData),
            { NewUserData, State#state{ user_channels=UserChannels#{ UserId => NewUserData } } }
    end.
