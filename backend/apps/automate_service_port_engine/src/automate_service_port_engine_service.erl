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
-include("../../automate_bot_engine/src/program_records.hrl").

%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    ignore.

%% No monitor associated with this service
get_monitor_id(UserId, [ServicePortId]) ->
    ?BACKEND:get_or_create_monitor_id(UserId, ServicePortId).

-spec call(binary(), any(), #program_thread{}, binary(), _) -> {ok, #program_thread{}, any()}.
call(FunctionName, Values, Thread, UserId, [ServicePortId]) ->
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServicePortId,
                                                                              UserId),
    {ok, MonitorId } = automate_service_registry_query:get_monitor_id(Module, UserId),
    LastMonitorValue = case automate_bot_engine_variables:get_last_monitor_value(
                              Thread, MonitorId) of
                           {ok, Value} -> Value;
                           {error, not_found} -> null
                       end,

    {ok, ObfuscatedUserId} = automate_service_port_engine:internal_user_id_to_service_port_user_id(UserId, ServicePortId),
    {ok, #{ <<"result">> := Result }} = automate_service_port_engine:call_service_port(
                                          ServicePortId,
                                          FunctionName,
                                          Values,
                                          ObfuscatedUserId,
                                          #{ <<"last_monitor_value">> => LastMonitorValue}),
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
