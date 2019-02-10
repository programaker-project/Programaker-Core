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
               }).

init(Req, Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ServicePortId = cowboy_req:binding(service_port_id, Req),

    {cowboy_websocket, Req, #state{ service_port_id=ServicePortId
                                  , user_id=UserId
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

websocket_handle(Message, State) ->
    {ok, State}.

%% automate_service_port_engine:call_service_port(<<"d9c566da-ca2b-4fb8-95a3-702ba5c9abbb">>, <<"__ping">>, []).
websocket_info(Message, State) ->
    io:fwrite("Got ~p~n", [Message]),
    {reply, {binary, Message}, State}.
