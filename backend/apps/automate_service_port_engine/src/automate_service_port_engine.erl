%%%-------------------------------------------------------------------
%% @doc automate_service_port_engine APP API
%% @end
%%%-------------------------------------------------------------------

%% @doc automate_service_port_engine APP API
-module(automate_service_port_engine).

%% Application callbacks
-export([ create_service_port/2
        , register_service_port/1
        , from_service_port/2
        , ask_service_port/2
        ]).

-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(ROUTER, automate_service_port_engine_router).

%%====================================================================
%% API
%%====================================================================

-spec create_service_port(binary(), boolean()) -> {ok, binary()} | {error, term(), string()}.
create_service_port(UserId, ServicePortName) ->
    ?BACKEND:create_service_port(UserId, ServicePortName).

-spec register_service_port(binary()) -> ok.
register_service_port(ServicePortId) ->
    ChannelId = ServicePortId,
    Process = self(),
    ?ROUTER:open_outbound_channel({to_service, ChannelId},
                                  fun(Msg) ->
                                          Process ! Msg
                                  end),
    ok.

-spec ask_service_port(binary(), binary()) -> ok.
ask_service_port(ServicePortId, Msg) ->
    ChannelId = ServicePortId,
    ?ROUTER:route_inbound({to_service, ChannelId}, Msg),
    ok.

-spec from_service_port(binary(), binary()) -> ok.
from_service_port(ServicePortId, Msg) ->
    ChannelId = ServicePortId,
    ?ROUTER:route_inbound({from_service, ChannelId}, Msg),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
