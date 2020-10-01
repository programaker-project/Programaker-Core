%%%-------------------------------------------------------------------
%% @doc Timekeeping service main module.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_service).

%% Service API
-export([ start_link/0
        , is_enabled_for_user/2
        , get_how_to_enable/2
        , listen_service/3
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

-spec listen_service(owner_id(), {binary() | undefined, binary() | undefined}, [binary(), ...]) -> ok | {error, no_valid_connection}.
listen_service(Owner, {Key, SubKey}, [ServicePortId]) ->
    case get_connection(Owner, ServicePortId, {Key, SubKey}) of
        {ok, ConnectionId} ->
            {ok, ConnectionOwner} = ?BACKEND:get_connection_owner(ConnectionId),
            {ok, ChannelId} = ?BACKEND:get_or_create_monitor_id(ConnectionOwner, ServicePortId),
            automate_channel_engine:listen_channel(ChannelId, {Key, SubKey});
        {error, not_found} ->
            {error, no_valid_connection}
    end.

-spec call(binary(), any(), #program_thread{}, owner_id(), _) -> {ok, #program_thread{}, any()}.
call(FunctionId, Values, Thread=#program_thread{program_id=ProgramId}, Owner, [ServicePortId]) ->
    {ok, MonitorId } = ?BACKEND:get_or_create_monitor_id(Owner, ServicePortId),
    LastMonitorValue = case automate_bot_engine_variables:get_last_monitor_value(
                              Thread, MonitorId) of
                           {ok, Value} -> Value;
                           {error, not_found} -> null
                       end,

    ConnectionId = case automate_bot_engine_variables:get_thread_context(Thread) of
                       { ok, #{ bridge_connection := #{ ServicePortId := ContextConnectionId } } } ->
                           ContextConnectionId;
                       _ ->
                           {ok, BlockInfo} = ?BACKEND:get_block_definition(ServicePortId, FunctionId),
                           {Key, SubKey} = case get_block_resource(BlockInfo, Values) of
                               {ok, {Resource, ResourceValue}} ->
                                   {Resource, ResourceValue};
                               {error, not_found} ->
                                   {undefined, undefined}
                           end,
                           case get_connection(Owner, ServicePortId, {Key, SubKey}) of
                               {ok, AvailableConnection} ->
                                   AvailableConnection
                               end
                   end,

    case automate_service_port_engine:call_service_port(
           ServicePortId,
           FunctionId,
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
-spec get_connection(Owner :: owner_id(), ServicePortId :: binary(), { binary() | undefined, binary() | undefined })
                    -> {ok, binary()} | {error, not_found}.
get_connection(Owner, ServicePortId, {Resource, ResourceValue}) ->
    case automate_service_port_engine:internal_user_id_to_connection_id(Owner, ServicePortId) of
        {ok, DefaultConnectionId} ->
            {ok, DefaultConnectionId};
        {error, not_found} ->
            {ok, Shares} = automate_service_port_engine:get_resources_shared_with_on_bridge(Owner, ServicePortId),
            MatchingConnections = lists:filter(fun(#bridge_resource_share_entry{ connection_id=SharedConnectionId
                                                                               , resource=SharedResource
                                                                               , value=SharedResourceValue
                                                                               }) ->
                                                       (Resource == SharedResource) and (ResourceValue == SharedResourceValue)
                                               end, Shares),
            case MatchingConnections of
                [#bridge_resource_share_entry{ connection_id=SharedConnectionId } | _] ->
                    {ok, SharedConnectionId};
                [] ->
                    {error, not_found}
            end
    end.

-spec get_block_resource(BlockInfo :: #service_port_block{}, Values :: [ any() ])
                        -> {ok, { binary(), binary()}} | {error, not_found}.
get_block_resource(BlockInfo=#service_port_block{ arguments=Args }, Values) ->
    get_block_resource_aux(Args, Values).

get_block_resource_aux([ #service_port_block_collection_argument{ name=Name } | _ ], [ Value | _ ]) ->
    {ok, {Name, Value}};
get_block_resource_aux([], []) ->
    {error, not_found};
get_block_resource_aux([ _ | TArg ], [ _ | TValue ]) ->
    get_block_resource_aux(TArg, TValue).
