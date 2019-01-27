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
        ]).

%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    ignore.

%% No monitor associated with this service
get_monitor_id(_UserId, _Params) ->
    {error, not_found}.

call(FunctionName, Values, Thread, UserId, [ServicePortId]) ->
    {ok, ObfuscatedUserId} = automate_service_port_engine:internal_user_id_to_service_port_user_id(UserId, ServicePortId),
    {ok, #{ <<"result">> := Result }} = automate_service_port_engine:call_service_port(ServicePortId, FunctionName, Values, ObfuscatedUserId),
    {ok, Thread, Result}.

%% Is enabled for all users
is_enabled_for_user(_Username, _Params) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(_Username, _Params) ->
    {error, not_found}.
