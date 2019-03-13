%%%-------------------------------------------------------------------
%% @doc Timekeeping service main module.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_service).

%% Service API
-export([ start_link/0
        , is_enabled_for_user/2
        , get_how_to_enable/2
        , get_monitor_id/2
        , call/5
        , send_registration_data/3
        ]).

-define(BACKEND, automate_service_port_engine_mnesia_backend).

%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    ignore.

%% No monitor associated with this service
get_monitor_id(UserId, [ServicePortId]) ->
    ?BACKEND:get_or_create_monitor_id(UserId, ServicePortId).

call(FunctionName, Values, Thread, UserId, [ServicePortId]) ->
    {ok, ObfuscatedUserId} = automate_service_port_engine:internal_user_id_to_service_port_user_id(UserId, ServicePortId),
    {ok, #{ <<"result">> := Result }} = automate_service_port_engine:call_service_port(ServicePortId, FunctionName, Values, ObfuscatedUserId),
    {ok, Thread, Result}.

%% Is enabled for all users
is_enabled_for_user(_Username, _Params) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(#{ user_id := UserId }, [ServicePortId]) ->
    {ok, ObfuscatedUserId} = automate_service_port_engine:internal_user_id_to_service_port_user_id(UserId, ServicePortId),
    {ok, #{ <<"result">> := Result }} = automate_service_port_engine:get_how_to_enable(ServicePortId, ObfuscatedUserId),
    {ok, Result}.

send_registration_data(UserId, RegistrationData, [ServicePortId]) ->
    {ok, ObfuscatedUserId} = automate_service_port_engine:internal_user_id_to_service_port_user_id(UserId, ServicePortId),
    {ok, Result} = automate_service_port_engine:send_registration_data(ServicePortId, RegistrationData, ObfuscatedUserId),
    {ok, Result}.
