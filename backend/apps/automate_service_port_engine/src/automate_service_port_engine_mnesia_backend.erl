%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_mnesia_backend).

-export([ start_link/0
        , create_service_port/2
        , set_service_port_configuration/3
        ]).

-include("records.hrl").
-define(SERVICE_PORT_TABLE, automate_service_port_table).
-define(SERVICE_PORT_CONFIGURATION_TABLE, automate_service_port_configuration_table).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    Nodes = [node()],

    %% Service port identity table
    ok = case mnesia:create_table(?SERVICE_PORT_TABLE,
                                  [ { attributes, record_info(fields, service_port_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    %% Service port configuration table
    ok = case mnesia:create_table(?SERVICE_PORT_CONFIGURATION_TABLE,
                                  [ { attributes, record_info(fields, service_port_configuration)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_configuration }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ignore.

-spec create_service_port(binary(), boolean()) -> {ok, binary()} | {error, term(), string()}.
create_service_port(UserId, ServicePortName) ->
    ServicePortId = generate_id(),
    Entry = #service_port_entry{ id=ServicePortId
                               , name=ServicePortName
                               , owner=UserId
                               },

    Transaction = fun() ->
                          ok = mnesia:write(?SERVICE_PORT_TABLE, Entry, write)
                  end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} ->
            {ok, ServicePortId};
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

get_service_id_for_port(ServicePortId) ->
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, ServicePortId) of
                              [] ->
                                  {error, not_found};
                              [#service_port_configuration{service_id=ServiceId}] ->
                                  {ok, ServiceId}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

create_service_for_port(Configuration, OwnerId) ->
    case Configuration of
        #service_port_configuration{ is_public=true } ->
            automate_service_registry:register_public(as_module(Configuration));
        _ ->
            {ok, ServiceId} = automate_service_registry:register_private(as_module(Configuration)),
            ok = automate_service_registry:allow_user(ServiceId, OwnerId),
            {ok, ServiceId}
    end.

as_module(#service_port_configuration{ id=Id
                                     , service_name=Name
                                     }) ->
    #{ name => Name
     , uuid => Id
     , description => <<"A service port configuration">>
     , module => {automate_service_port_engine_service, [Id]}
     }.

set_service_port_configuration(ServicePortId, Configuration, OwnerId) ->
    io:fwrite("Setting configuration: ~p~n", [Configuration]),

    ServiceId = case get_service_id_for_port(ServicePortId) of
                    {ok, FoundServiceId} ->
                        FoundServiceId;
                    {error, not_found} ->
                        {ok, NewServiceId} = create_service_for_port(Configuration, OwnerId),
                        NewServiceId
                end,

    Transaction = fun() ->
                          mnesia:write(?SERVICE_PORT_CONFIGURATION_TABLE
                                      , Configuration#service_port_configuration{ service_id=ServiceId }
                                      , write
                                      )
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
