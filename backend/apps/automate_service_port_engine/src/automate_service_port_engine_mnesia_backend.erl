%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_mnesia_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_port_engine_mnesia_backend).

-export([ start_link/0
        , create_service_port/2
        , set_service_port_configuration/3

        , list_custom_blocks/1
        , internal_user_id_to_service_port_user_id/2
        ]).

-include("records.hrl").
-define(SERVICE_PORT_TABLE, automate_service_port_table).
-define(SERVICE_PORT_CONFIGURATION_TABLE, automate_service_port_configuration_table).
-define(SERVICE_PORT_USERID_OBFUSCATION_TABLE, automate_service_port_userid_obfuscation_table).
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

    %% Service port userId obfuscation
    ok = case mnesia:create_table(?SERVICE_PORT_USERID_OBFUSCATION_TABLE,
                                  [ { attributes, record_info(fields, service_port_user_obfuscation_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_port_user_obfuscation_entry }
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

-spec list_custom_blocks(binary()) -> {ok, [_]}.
list_custom_blocks(UserId) ->
    Transaction = fun() ->
                          Services = list_userid_ports(UserId),
                          {ok
                          , maps:from_list(
                              lists:filter(fun (X) -> X =/= none end,
                                           lists:map(fun (PortId) ->
                                                             list_blocks_for_port(PortId)
                                                     end,
                                                     Services)))}
                  end,
    case mnesia:transaction(Transaction) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec internal_user_id_to_service_port_user_id(binary(), binary()) -> {ok, binary()}.
internal_user_id_to_service_port_user_id(UserId, ServicePortId) ->
    FullId = {UserId, ServicePortId},
    Transaction = fun() ->
                          case mnesia:read(?SERVICE_PORT_USERID_OBFUSCATION_TABLE, FullId) of
                              [] ->
                                  NewId = generate_id(),
                                  ok = mnesia:write(?SERVICE_PORT_USERID_OBFUSCATION_TABLE,
                                                    #service_port_user_obfuscation_entry{ id=FullId
                                                                                        , obfuscated_id=NewId
                                                                                        }, write),
                                  {ok, NewId};
                              [#service_port_user_obfuscation_entry{ obfuscated_id=ObfuscatedId }] ->
                                  {ok, ObfuscatedId}
                          end
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
list_userid_ports(UserId) ->
    MatchHead = #service_port_entry{ id='$1'
                                   , name='_'
                                   , owner='$2'
                                   , service_id='_'
                                   },
    Guard = {'==', '$2', UserId},
    ResultColumn = '$1',
    Matcher = [{MatchHead, [Guard], [ResultColumn]}],

    mnesia:select(?SERVICE_PORT_TABLE, Matcher).

list_blocks_for_port(PortId) ->
    case mnesia:read(?SERVICE_PORT_CONFIGURATION_TABLE, PortId) of
        [] -> none;
        [#service_port_configuration{ blocks=Blocks
                                    , service_id=ServiceId
                                    }] ->
            {ServiceId, Blocks}
    end.

generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
