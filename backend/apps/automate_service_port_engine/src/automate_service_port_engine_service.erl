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
-include("./records.hrl").

%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    ignore.

%% No monitor associated with this service
-spec get_monitor_id(owner_id(), [binary(), ...]) -> {ok, binary()} | {error, _, binary()}.
get_monitor_id(Owner, [ServicePortId]) when is_binary(ServicePortId) ->
    {ok, ConnectionId} = get_connection(Owner, ServicePortId),
    {ok, ConnectionOwner} = ?BACKEND:get_connection_owner(ConnectionId),
    ?BACKEND:get_or_create_monitor_id(ConnectionOwner, ServicePortId).

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
                           case get_connection(Owner, ServicePortId) of
                               {ok, AvailableConnection} ->
                                   AvailableConnection
                               end
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
is_enabled_for_user(_Owner, _Params) ->
    {ok, true}.

%% No need to enable service
-spec get_how_to_enable(owner_id(), [binary()]) -> {ok, map()} | {error, not_found}.
get_how_to_enable(Owner, [ServicePortId]) ->
    {ok, TemporaryConnectionId} = ?BACKEND:gen_pending_connection(ServicePortId, Owner),
    case automate_service_port_engine:get_how_to_enable(ServicePortId, TemporaryConnectionId) of
        {error, Err} ->
            {error, Err};
        {ok, Response} ->
            case Response of
                #{ <<"result">> := null } ->
                    {ok, #{ <<"type">> => <<"direct">> } };
                #{ <<"result">> := Result } ->
                    {ok, Result#{ <<"connection_id">> => TemporaryConnectionId }};
                _ ->
                    {ok, #{ <<"type">> => <<"direct">> } }

            end
    end.

-spec send_registration_data(owner_id(), any(), [binary()], map()) -> {ok, any()}.
send_registration_data(Owner, RegistrationData, [ServicePortId], Properties) ->
    ConnectionId = case Properties of
                       #{ <<"connection_id">> := ConnId } when is_binary(ConnId) -> ConnId;
                       _ ->
                           {ok, TemporaryConnectionId} = ?BACKEND:gen_pending_connection(ServicePortId, Owner),
                           TemporaryConnectionId
                   end,

    {ok, Result} = automate_service_port_engine:send_registration_data(ServicePortId, RegistrationData, ConnectionId),
    PassedResult = case Result of
                       #{ <<"success">> := true } ->
                           Name = get_name_from_result(Result),
                           ok = ?BACKEND:establish_connection(ServicePortId, Owner, ConnectionId, Name),
                           Result;

                       #{ <<"success">> := false, <<"error">> := <<"No registerer available">> } ->
                           %% For compatibility with plaza/programaker-bridge library before connections
                           %% where introduced.
                           Name = get_name_from_result(Result),
                           ok = ?BACKEND:establish_connection(ServicePortId, Owner, ConnectionId, Name),
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


%%====================================================================
%% Internal
%%====================================================================
-spec get_connection(Owner :: owner_id(), ServicePortId :: binary()) -> {ok, binary()} | {error, not_found}.
get_connection(Owner, ServicePortId) ->
    case automate_service_port_engine:internal_user_id_to_connection_id(Owner, ServicePortId) of
        {ok, DefaultConnectionId} ->
            {ok, DefaultConnectionId};
        {error, not_found} ->
            case automate_service_port_engine:get_resources_shared_with_on_bridge(Owner, ServicePortId) of
                {ok, [#bridge_resource_share_entry{ connection_id=SharedConnectionId } | _]} ->
                    %% TODO: Capture the connection which is really needed, and if it's allowed
                    {ok, SharedConnectionId};
                {ok, [] }->
                    {error, not_found}
            end
    end.
