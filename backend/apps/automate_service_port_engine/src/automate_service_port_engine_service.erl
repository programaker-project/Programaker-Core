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
        , send_registration_data/4
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
-spec get_monitor_id(owner_id(), [binary(), ...]) -> {ok, binary} | {error, _, binary()}.
get_monitor_id(Owner, [ServicePortId]) ->
    ?BACKEND:get_or_create_monitor_id(Owner, ServicePortId).

-spec call(binary(), any(), #program_thread{}, owner_id(), _) -> {ok, #program_thread{}, any()}.
call(FunctionName, Values, Thread=#program_thread{program_id=ProgramId}, Owner, [ServicePortId]) ->
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServicePortId),
    {ok, MonitorId } = automate_service_registry_query:get_monitor_id(Module, Owner),
    LastMonitorValue = case automate_bot_engine_variables:get_last_monitor_value(
                              Thread, MonitorId) of
                           {ok, Value} -> Value;
                           {error, not_found} -> null
                       end,

    ConnectionId = case automate_bot_engine_variables:get_thread_context(Thread) of
                       { ok, #{ bridge_connection := #{ ServicePortId := ContextConnectionId } } } ->
                           ContextConnectionId;
                       _ ->
                           {ok, DefaultConnectionId} = automate_service_port_engine:internal_user_id_to_connection_id(Owner, ServicePortId),
                           DefaultConnectionId
                   end,

    case automate_service_port_engine:call_service_port(
           ServicePortId,
           FunctionName,
           Values,
           ConnectionId,
           #{ <<"last_monitor_value">> => LastMonitorValue}) of
        {ok, #{ <<"result">> := Result }} ->
            ok = automate_storage:mark_successful_call_to_bridge(ProgramId, ServicePortId),
            {ok, Thread, Result};
        {error, Reason} ->
            ok = automate_storage:mark_failed_call_to_bridge(ProgramId, ServicePortId),
            {error, Reason}
    end.

%% Is enabled for all users
is_enabled_for_user(_Username, _Params) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(#{ user_id := UserId }, [ServicePortId]) ->
    {ok, TemporaryConnectionId} = ?BACKEND:gen_pending_connection(ServicePortId, UserId),
    {ok, Response} = automate_service_port_engine:get_how_to_enable(ServicePortId, TemporaryConnectionId),
    case Response of
        #{ <<"result">> := null } ->
            {ok, #{ <<"type">> => <<"direct">> } };
        #{ <<"result">> := Result } ->
            {ok, Result#{ <<"connection_id">> => TemporaryConnectionId }};
        _ ->
            {ok, #{ <<"type">> => <<"direct">> } }
    end.

send_registration_data(UserId, RegistrationData, [ServicePortId], Properties) ->
    ConnectionId = case Properties of
                       #{ <<"connection_id">> := ConnId } when is_binary(ConnId) -> ConnId;
                       _ ->
                           {ok, TemporaryConnectionId} = ?BACKEND:gen_pending_connection(ServicePortId, UserId),
                           TemporaryConnectionId
                   end,

    {ok, Result} = automate_service_port_engine:send_registration_data(ServicePortId, RegistrationData, ConnectionId),
    PassedResult = case Result of
                       #{ <<"success">> := true } ->
                           Name = get_name_from_result(Result),
                           ok = ?BACKEND:establish_connection(ServicePortId, UserId, ConnectionId, Name),
                           Result;

                       #{ <<"success">> := false, <<"error">> := <<"No registerer available">> } ->
                           %% For compatibility with plaza/programaker-bridge library before connections
                           %% where introduced.
                           Name = get_name_from_result(Result),
                           ok = ?BACKEND:establish_connection(ServicePortId, UserId, ConnectionId, Name),
                           Result#{ <<"success">> => true
                                  , <<"error">> => null
                                  };

                       _ ->
                           Result
         end,
    {ok, PassedResult}.

get_name_from_result(#{ <<"data">> := #{ <<"name">> := Name } }) ->
    Name;
get_name_from_result(_) ->
    undefined.
