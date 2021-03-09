%%%-------------------------------------------------------------------
%% @doc automate_service_port_engine APP API
%% @end
%%%-------------------------------------------------------------------

%% @doc automate_service_port_engine APP API
-module(automate_service_port_engine).

%% Application callbacks
-export([ create_service_port/2
        , register_service_port/1
        , from_service_port/3
        , call_service_port/5
        , get_how_to_enable/2
        , send_registration_data/3
        , send_oauth_return/2

        , list_custom_blocks/1
        , internal_user_id_to_connection_id/2
        , get_user_service_ports/1
        , delete_bridge/2
        , callback_bridge/4
        , callback_bridge_through_connection/4
        , get_channel_origin_bridge/1
        , get_bridge_info/1
        , get_bridge_owner/1
        , get_bridge_configuration/1

        , listen_bridge/2
        , listen_bridge/3
        , list_established_connections/1
        , list_established_connections/2
        , get_pending_connection_info/1
        , is_module_connectable_bridge/2

        , set_shared_resource/3
        , get_connection_owner/1
        , get_connection_shares/1
        , get_connection_bridge/1
        , get_resources_shared_with/1
        , get_resources_shared_with_on_bridge/2

        , create_bridge_token/4
        , list_bridge_tokens/1
        , delete_bridge_token_by_name/2
        , check_bridge_token/2
        , can_skip_authentication/1
        , set_save_signals_on_connection/3
        ]).

-include("records.hrl").

-define(BACKEND, automate_service_port_engine_mnesia_backend).
-define(ROUTER, automate_service_port_engine_router).
-define(LOGGING, automate_logging).
-include("router_error_cases.hrl").

%%====================================================================
%% API
%%====================================================================

-spec create_service_port(owner_id(), binary()) -> {ok, binary()} | {error, term(), string()}.
create_service_port(Owner, ServicePortName) when is_tuple(Owner) ->
    ?BACKEND:create_service_port(Owner, ServicePortName).

-spec register_service_port(binary()) -> ok.
register_service_port(ServicePortId) ->
    ?ROUTER:connect_bridge(ServicePortId).

-spec call_service_port(binary(), binary(), any(), owner_id() | binary(), map()) -> {ok, map()} | {error, ?ROUTER_ERROR_CLASSES}.
call_service_port(ServicePortId, FunctionName, Arguments, Owner, ExtraData) when is_tuple(Owner) ->
    case internal_user_id_to_connection_id(Owner, ServicePortId) of
        {ok, ConnectionId} ->
            call_service_port(ServicePortId, FunctionName, Arguments, ConnectionId, ExtraData);
        {error, Reason} ->
            {error, Reason}
    end;

call_service_port(ServicePortId, FunctionName, Arguments, ConnectionId, ExtraData) ->
    ?LOGGING:log_call_to_bridge(ServicePortId, FunctionName, Arguments, ConnectionId, ExtraData),

    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"FUNCTION_CALL">>
                                        , <<"user_id">> => ConnectionId
                                        , <<"value">> => #{ <<"function_name">> => FunctionName
                                                          , <<"arguments">> => Arguments
                                                          }
                                        , <<"extra_data">> => ExtraData
                                        }).

-spec get_how_to_enable(binary(), binary()) -> {ok, any()} | {error, atom()}.
get_how_to_enable(ServicePortId, ConnectionId) ->
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"GET_HOW_TO_SERVICE_REGISTRATION">>
                                        , <<"user_id">> => ConnectionId
                                        , <<"value">> => #{}
                                        }).

-spec send_registration_data(binary(), map(), binary()) -> {ok, map()}.
send_registration_data(ServicePortId, Data, ConnectionId) ->
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"REGISTRATION">>
                                        , <<"user_id">> => ConnectionId
                                        , <<"value">> => #{ <<"form">> => Data }
                                        }).

-spec send_oauth_return(binary(), binary()) -> {ok, map()} | {error, term()}.
send_oauth_return(Qs, ServicePortId) ->
    ?ROUTER:call_bridge(ServicePortId, #{ <<"type">> => <<"OAUTH_RETURN">>
                                        , <<"value">> => #{ <<"query_string">> => Qs }
                                        }).

-spec listen_bridge(binary(), owner_id()) -> ok | {error, term()}.
listen_bridge(BridgeId, Owner) when is_tuple(Owner) ->
    listen_bridge(BridgeId, Owner, {undefined, undefined}).

-spec listen_bridge(binary(), owner_id(), {binary()} | {binary() | undefined, binary() | undefined}) -> ok | {error, term()}.
listen_bridge(BridgeId, Owner, Selector) when is_tuple(Owner) ->
    case Selector of
        {Key} ->
            automate_service_port_engine_service:listen_service(Owner, {Key, undefined}, [BridgeId]);
        {Key, SubKey} ->
            automate_service_port_engine_service:listen_service(Owner, {Key, SubKey}, [BridgeId])
    end.

-spec from_service_port(binary(), owner_id(), map()) -> ok.
from_service_port(ServicePortId, Owner, Unpacked) when is_tuple(Owner) ->
    automate_stats:log_observation(counter,
                                   automate_bridge_engine_messages_from_bridge,
                                   [ServicePortId]),
    case Unpacked of
        %% This has to be first because of the use of MessageId here
        AdviceMsg = #{ <<"type">> := <<"ADVICE_SET">>, <<"message_id">> := MessageId } ->
            AdviceTaken = apply_advice(AdviceMsg, ServicePortId),
            answer_advice_taken(AdviceTaken, MessageId, self());

        #{ <<"message_id">> := MessageId } ->
            %% io:fwrite("[~p] Answer: ~p~n", [MessageId, Unpacked]),
            ?ROUTER:answer_message(MessageId, Unpacked);

        #{ <<"type">> := <<"CONFIGURATION">>
         , <<"value">> := Configuration
         } ->
            {ok, Todo} = set_service_port_configuration(ServicePortId, Configuration, Owner),
            %% TODO: Check that it really exists, don't trust the DB
            case lists:member(request_icon, Todo) of
                false -> ok;
                true ->
                    %% Request icon
                    request_icon(self())
            end;

        #{ <<"type">> := <<"ICON_UPLOAD">>
         , <<"value">> := IconData
         } ->
            case IconData of
                #{ <<"content">> := B64Content } ->
                    Data = base64:decode(B64Content),
                    ok = write_icon(Data, ServicePortId)
            end;

        #{ <<"type">> := <<"ESTABLISH_CONNECTION">>
         , <<"value">> := #{ <<"connection_id">> := ConnectionId
                           , <<"name">> := Name
                           }
         } ->
            case ?BACKEND:establish_connection(ServicePortId, ConnectionId, Name) of
                ok ->
                    io:fwrite("[~p] Established connection: ~p~n", [ServicePortId, ConnectionId]);
                {error, Reason} ->
                    io:fwrite("[~p] Tried to establish connection but failed: ~p~n", [ServicePortId, Reason])
            end;

        Notif=#{ <<"type">> := <<"NOTIFICATION">>
               , <<"key">> := Key
               , <<"to_user">> := ToUser
               , <<"value">> := Value
               , <<"content">> := Content
               } ->
            case ToUser of
                null ->
                    {ok, Connections} = ?BACKEND:list_bridge_connections(ServicePortId),
                    Results = lists:map(fun (#user_to_bridge_connection_entry{ channel_id=ChannelId
                                                                             , owner=ConnectionOwner
                                                                             , save_signals=Save
                                                                             }) ->
                                                case Save of
                                                    true -> ?LOGGING:log_signal_to_bridge_and_owner(Notif, ServicePortId, ConnectionOwner);
                                                    false -> ok
                                                end,
                                                { ChannelId
                                                , automate_channel_engine:send_to_channel(
                                                    ChannelId,
                                                    #{ <<"key">> => Key
                                                     , <<"value">> => Value
                                                     , <<"content">> => Content
                                                     , <<"subkey">> => get_subkey_from_notification(Notif)
                                                     , <<"service_id">> => ServicePortId
                                                     })}
                                        end, Connections),
                    lists:foreach(
                      fun({Channel, Result}) ->
                              case Result of
                                  ok -> ok;
                                  Err -> io:fwrite("Error sending to channel (~p): ~p~n", [Channel, Err])
                              end
                      end, Results),
                    %% Make sure to crash if there's an error, but only after the
                    %% messages had been sent
                    ok;
                _ ->
                    case ?BACKEND:get_connection_by_id(ToUser) of
                        {ok, #user_to_bridge_connection_entry{channel_id=ChannelId, bridge_id=ServicePortId, save_signals=Save}} ->
                            case Save of
                                true ->
                                    ?LOGGING:log_signal_to_bridge_and_owner(Notif, ServicePortId, Owner);
                                false ->
                                    ok
                            end,

                            case automate_channel_engine:send_to_channel(ChannelId,
                                                                         #{ <<"key">> => Key
                                                                          , <<"value">> => Value
                                                                          , <<"content">> => Content
                                                                          , <<"subkey">> => get_subkey_from_notification(Notif)
                                                                          , <<"service_id">> => ServicePortId
                                                                          }) of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    automate_logging:log_platform(
                                      error,
                                      io_lib:format("[~p] Error propagating notification: ~p  (conn: ~p, monitor_id: ~p)~n",
                                                    [ServicePortId, Reason, ToUser, ChannelId]))
                            end;
                        {error, Reason} ->
                            automate_logging:log_platform(
                              error,
                              io_lib:format("[~p] Error propagating notification (to ~p): ~p~n", [ServicePortId, ToUser, Reason]));
                        {ok, #user_to_bridge_connection_entry{bridge_id=OtherServicePortId}} ->
                            automate_logging:log_platform(
                              error,
                              io_lib:format("[~p] BridgeId ~p sent message to conenction with bridgeId ~p~n",
                                            [?MODULE, ServicePortId, OtherServicePortId]))
                    end
            end
    end.

-spec list_custom_blocks(owner_id()) -> {ok, map()}.
list_custom_blocks(Owner) when is_tuple(Owner) ->
    ?BACKEND:list_custom_blocks(Owner).

-spec internal_user_id_to_connection_id(owner_id(), binary()) -> {ok, binary()} | {error, not_found} | {error, any()}.
internal_user_id_to_connection_id(Owner, ServicePortId) when is_tuple(Owner) ->
    ?BACKEND:internal_user_id_to_connection_id(Owner, ServicePortId).


-spec get_user_service_ports(owner_id()) -> {ok, [#service_port_entry_extra{}]}.
get_user_service_ports(Owner) when is_tuple(Owner) ->
    {ok, Bridges} = ?BACKEND:get_user_service_ports(Owner),
    {ok, lists:map(fun add_service_port_extra/1, Bridges)}.

-spec delete_bridge(owner_id(), binary()) -> ok | {error, binary()}.
delete_bridge(Accessor, BridgeId) when is_tuple(Accessor) ->
    ok = case ?BACKEND:get_service_id_for_port(BridgeId) of
             {error, not_found} ->
                 ok;
             {ok, ServiceId} ->
                 automate_service_registry:delete_service(Accessor, ServiceId)
         end,
    ?BACKEND:delete_bridge(Accessor, BridgeId).


-spec callback_bridge(owner_id(), binary(), binary(), undefined | binary()) -> {ok, map() | [#{ id => binary(), name => binary() }]} | {error, term()}.
callback_bridge(Owner, BridgeId, CallbackName, SequenceId) when is_tuple(Owner) ->
    case internal_user_id_to_connection_id(Owner, BridgeId) of
        {ok, ConnectionId} ->
            case callback_bridge_through_connection(ConnectionId, BridgeId, CallbackName, SequenceId) of
                {ok, #{ <<"result">> := Result } } ->
                    {ok, Result};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            case ?BACKEND:is_user_connected_to_bridge(Owner, BridgeId) of
                {ok, false} ->
                    {error, not_found};
                {ok, true, _Values} ->
                    %% No direct connection, but still connected (via shared connection)
                    %% We can pull the values from the share
                    %% TODO: Reformat values from the _Values already returned
                    {ok, Shares} = ?BACKEND:get_resources_shared_with(Owner),
                    Values = lists:filtermap(fun(#bridge_resource_share_entry{ connection_id=ConnectionId
                                                                             , resource=Resource
                                                                             , value=Value
                                                                             , name=Name
                                                                             }) ->
                                                     case Resource of
                                                         CallbackName ->
                                                             case get_connection_bridge(ConnectionId) of
                                                                 {ok, BridgeId} ->
                                                                     {true, #{ id => Value, name => Name}};
                                                                 _ ->
                                                                     false
                                                             end;
                                                         _ ->
                                                             false
                                                     end
                                             end, Shares),
                    {ok, Values}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_bridge_through_connection(binary(), binary(), binary(), undefined | binary()) -> {ok, map()} | {error, term()}.
callback_bridge_through_connection(ConnectionId, BridgeId, CallbackName, SequenceId) ->
    ?ROUTER:call_bridge(BridgeId, #{ <<"type">> => <<"CALLBACK">>
                                   , <<"user_id">> => ConnectionId
                                   , <<"value">> => #{ <<"callback">> => CallbackName
                                                     , <<"sequence_id">> => case SequenceId of
                                                                                undefined -> null;
                                                                                _ -> SequenceId
                                                                            end
                                                     }
                                   }).


-spec get_channel_origin_bridge(binary()) -> {ok, binary()} | {error, not_found}.
get_channel_origin_bridge(ChannelId) ->
    case automate_services_time:get_monitor_id() of
        {ok, ChannelId} ->
            {ok, automate_services_time:get_uuid()};
        _ ->
            ?BACKEND:get_channel_origin_bridge(ChannelId)
    end.

-spec get_bridge_info(binary()) -> {ok, #service_port_metadata{}} | {error, not_found}.
get_bridge_info(BridgeId) ->
    ?BACKEND:get_bridge_info(BridgeId).

-spec get_bridge_owner(binary()) -> {ok, owner_id()} | {error, not_found}.
get_bridge_owner(BridgeId) ->
    ?BACKEND:get_bridge_owner(BridgeId).

-spec get_bridge_configuration(binary()) -> {ok, #service_port_configuration{}} | {error, not_found}.
get_bridge_configuration(BridgeId) ->
    ?BACKEND:get_bridge_configuration(BridgeId).


-spec list_established_connections(owner_id()) -> {ok, [#user_to_bridge_connection_entry{}]}.
list_established_connections(Owner) when is_tuple(Owner) ->
    ?BACKEND:list_established_connections(Owner).

-spec list_established_connections(owner_id(), binary()) -> {ok, [#user_to_bridge_connection_entry{}]}.
list_established_connections(Owner, BridgeId) when is_tuple(Owner) ->
    ?BACKEND:list_established_connections(Owner, BridgeId).

-spec get_connection_owner(binary()) -> {ok, owner_id()} | {error, not_found}.
get_connection_owner(ConnectionId) ->
    ?BACKEND:get_connection_owner(ConnectionId).

-spec get_pending_connection_info(binary()) -> {ok, #user_to_bridge_pending_connection_entry{}}.
get_pending_connection_info(ConnectionId) ->
    ?BACKEND:get_pending_connection_info(ConnectionId).


-spec is_module_connectable_bridge(owner_id(), module() | {module(), any()}) ->
          false | {boolean(), {#service_port_entry{}, #service_port_configuration{}}}.
is_module_connectable_bridge(Owner, {automate_service_port_engine_service, [ BridgeId | _ ]}) when is_tuple(Owner) ->
    %% It *is* a bridge. Only remains to check if a new connection can be established.
    case ?BACKEND:get_all_bridge_info(BridgeId) of
        {error, _Reason} ->
            false;
        {ok, BridgeInfo, BridgeConfiguration} ->
            IsConnectable = case BridgeConfiguration of
                                undefined -> false;
                                #service_port_configuration{ allow_multiple_connections=true } ->
                                    true;
                                #service_port_configuration{ allow_multiple_connections=false } ->
                                    case ?BACKEND:is_user_connected_to_bridge(Owner, BridgeId) of
                                        {ok, true, _} ->
                                            false;
                                        {ok, false} ->
                                            true
                                    end
                            end,
            {IsConnectable, {BridgeInfo, BridgeConfiguration}}
    end;

is_module_connectable_bridge(_, _) ->
    %% Is not a bridge
    false.


-spec set_shared_resource(ConnectionId :: binary(), ResourceName :: binary(), Shares :: map()) -> ok.
set_shared_resource(ConnectionId, ResourceName, Shares) ->
    ?BACKEND:set_shared_resource(ConnectionId, ResourceName, Shares).

-spec get_connection_shares(ConnectionId :: binary()) -> {ok, #{ binary() => #{ binary() => [ owner_id() ] } } }.
get_connection_shares(ConnectionId) ->
    ?BACKEND:get_connection_shares(ConnectionId).

-spec get_connection_bridge(ConnectionId :: binary()) -> {ok, binary()} | {error, not_found}.
get_connection_bridge(ConnectionId) ->
    ?BACKEND:get_connection_bridge(ConnectionId).

-spec get_resources_shared_with(Owner :: owner_id()) -> {ok, [#bridge_resource_share_entry{}]}.
get_resources_shared_with(Owner) ->
    ?BACKEND:get_resources_shared_with(Owner).

-spec get_resources_shared_with_on_bridge(Owner :: owner_id(), BridgeId :: binary()) -> {ok, [#bridge_resource_share_entry{}]}.
get_resources_shared_with_on_bridge(Owner, BridgeId) ->
    {ok, Shares} = get_resources_shared_with(Owner),
    {ok, lists:filter(fun(#bridge_resource_share_entry{ connection_id=ConnectionId }) ->
                              {ok, SharedBridgeId} = automate_service_port_engine:get_connection_bridge(ConnectionId),
                              SharedBridgeId == BridgeId
                      end, Shares)}.



-spec create_bridge_token(BridgeId :: binary(), Owner :: owner_id(), TokenName :: binary(), ExpiresOn :: non_neg_integer() | undefined)
                         -> {ok, binary()} | {error, name_taken}.
create_bridge_token(BridgeId, Owner, TokenName, ExpiresOn) ->
    ?BACKEND:create_bridge_token(BridgeId, Owner, TokenName, ExpiresOn).

-spec list_bridge_tokens(BridgeId :: binary()) -> {ok, [#bridge_token_entry{}]}.
list_bridge_tokens(BridgeId) ->
    ?BACKEND:list_bridge_tokens(BridgeId).

-spec delete_bridge_token_by_name(BridgeId :: binary(), TokenName :: binary()) -> ok | {error, not_found}.
delete_bridge_token_by_name(BridgeId, TokenName) ->
    ?BACKEND:delete_bridge_token_by_name(BridgeId, TokenName).

-spec check_bridge_token(BridgeId :: binary(), Token :: binary()) -> {ok, boolean()}.
check_bridge_token(BridgeId, Token) ->
    ?BACKEND:check_bridge_token(BridgeId, Token).

-spec can_skip_authentication(BridgeId :: binary()) -> {ok, boolean()}.
can_skip_authentication(BridgeId) ->
    ?BACKEND:can_skip_authentication(BridgeId).

-spec set_save_signals_on_connection(ConnectionId :: binary(), Owner :: owner_id(), SaveSignals :: boolean()) -> ok | {error, _}.
set_save_signals_on_connection(ConnectionId, Owner, SaveSignals) ->
    ?BACKEND:set_save_signals_on_connection(ConnectionId, Owner, SaveSignals).

%%====================================================================
%% Internal functions
%%====================================================================

-spec add_service_port_extra({#service_port_entry{}, #service_port_configuration{}}) -> #service_port_entry_extra{}.
add_service_port_extra({#service_port_entry{ id=Id
                                           , name=Name
                                           , owner=Owner
                                           }, Config}) ->
    {ok, IsConnected} = ?ROUTER:is_bridge_connected(Id),

    BridgeIcon = case Config of
                     undefined -> undefined;
                     #service_port_configuration{ icon=Icon } -> Icon
                 end,
    #service_port_entry_extra{ id=Id
                             , name=Name
                             , owner=Owner
                             , is_connected=IsConnected
                             , icon=BridgeIcon
                             }.

set_service_port_configuration(ServicePortId, Configuration, Owner) ->
    SPConfiguration = parse_configuration_map(ServicePortId, Configuration, Owner),
    ?BACKEND:set_service_port_configuration(ServicePortId, SPConfiguration, Owner).

parse_configuration_map(ServicePortId,
                        Config=#{ <<"blocks">> := Blocks
                                , <<"is_public">> := RequestedPublic
                                , <<"service_name">> := ServiceName
                                }, Owner) ->
    Resources = parse_resources(Config),

    IsPublic = case RequestedPublic of
                   false ->
                       false;
                   true ->
                       case automate_storage:is_user_allowed_to_create_public_bridges(Owner) of
                           {ok, true} ->
                               true;
                           {ok, false} ->
                               automate_logging:log_platform(warning, list_to_binary(io_lib:format(
                                                                                       "[~p:~p] ~p tried to set a bridge to public (not allowed)",
                                                                                       [?MODULE, ?LINE, Owner]))),
                               false
                       end
               end,

    #service_port_configuration{ id=ServicePortId
                                 , is_public=IsPublic
                                 , service_id=undefined
                                 , service_name=ServiceName
                                 , blocks=lists:map(fun(B) -> parse_block(B) end, Blocks)
                                 , icon=get_icon_from_config(Config)
                                 , allow_multiple_connections=get_allow_multiple_connections_from_config(Config)
                                 , resources=lists:map(fun({Name, _Lockable}) -> Name end, Resources)
                                 }.

%% Find lockabel resources in configuration
-spec parse_resources(map()) -> [{ Name :: binary(), Lockable :: boolean() }].
parse_resources(#{ <<"resources">> := Resources }) ->
    lists:map(fun(Resource=#{ <<"name">> := Name }) ->
                      case Resource of
                          #{ <<"properties">> := #{  <<"lockable">> := true }
                           } ->
                              {Name, true};
                          _ ->  %% Not declared as lockable
                              {Name, false}
                      end
              end, Resources);

parse_resources(_) ->
    [].


-spec get_icon_from_config(map()) -> undefined | supported_icon_type().
get_icon_from_config(#{ <<"icon">> := #{ <<"url">> := Url } }) ->
    { url, Url };
get_icon_from_config(#{ <<"icon">> := #{ <<"sha256">> := Hash } }) ->
    Id={ hash, sha256, Hash },
    %% TODO: Check that ID exists, or request to bridge
    Id;
get_icon_from_config(_) ->
    undefined.

-spec get_allow_multiple_connections_from_config(map()) -> boolean().
get_allow_multiple_connections_from_config(#{ <<"allow_multiple_connections">> := AllowMultipleConnections })
  when is_boolean(AllowMultipleConnections) ->
    AllowMultipleConnections;
get_allow_multiple_connections_from_config(_) ->
    false.



parse_block(Block=#{ <<"arguments">> := Arguments
                   , <<"function_name">> := FunctionName
                   , <<"message">> := Message
                   , <<"id">> := BlockId
                   , <<"block_type">> := BlockType
                   , <<"block_result_type">> := BlockResultType
                   }) ->
    #service_port_block{ block_id=BlockId
                       , function_name=FunctionName
                       , message=Message
                       , arguments=lists:map(fun parse_argument/1, Arguments)
                       , block_type=BlockType
                       , block_result_type=BlockResultType
                       , save_to=get_block_save_to(Block)
                       };

parse_block(Block=#{ <<"arguments">> := Arguments
                             , <<"function_name">> := FunctionName
                             , <<"message">> := Message
                             , <<"id">> := BlockId
                             , <<"block_type">> := BlockType
                             , <<"save_to">> := SaveToConfiguration
                             , <<"expected_value">> := ExpectedValue
                             , <<"key">> := Key
                             }) ->
    #service_port_trigger_block{ block_id=BlockId
                               , function_name=FunctionName
                               , message=Message
                               , arguments=lists:map(fun parse_argument/1, Arguments)
                               , block_type=BlockType
                               , save_to=SaveToConfiguration
                               , expected_value=ExpectedValue
                               , key=Key
                               , subkey=get_block_subkey(Block)
                               }.

get_block_save_to(#{ <<"save_to">> := SaveTo }) ->
    SaveTo;
get_block_save_to(_) ->
    undefined.


get_block_subkey(#{ <<"subkey">> := SubKey }) ->
    SubKey;
get_block_subkey(_) ->
    undefined.

get_subkey_from_notification(#{ <<"subkey">> := SubKey }) ->
    SubKey;
get_subkey_from_notification(_) ->
    undefined.


parse_argument(#{ <<"default">> := DefaultValue
                , <<"type">> := Type
                }) ->
    #service_port_block_static_argument{ default=DefaultValue
                                       , type=Type
                                       , class=undefined
                                       };

parse_argument(Arg=#{ <<"type">> := <<"variable">>
                , <<"class">> := Class
                }) ->
    #service_port_block_static_argument{ type=get_variable_type(Arg)
                                       , class=Class
                                       , default=undefined
                                       };

parse_argument(Arg=#{ <<"type">> := <<"variable">>
                }) ->
    #service_port_block_static_argument{ type=get_variable_type(Arg)
                                       , default=undefined
                                       , class=undefined
                                       };
parse_argument(#{ <<"type">> := _Type
                , <<"values">> := #{ <<"collection">> := Collection
                                   }
                }) ->
    #service_port_block_collection_argument{ name=Collection };

parse_argument(#{ <<"type">> := Type
                , <<"values">> := #{ <<"callback">> := Callback
                                   }
                }) ->
    #service_port_block_dynamic_argument{ callback=Callback
                                        , type=Type
                                        };

parse_argument(#{ <<"type">> := Type
                , <<"values">> := #{ <<"callback_sequence">> := CallbackSequence
                                   }
                }) ->
    #service_port_block_dynamic_sequence_argument{ callback_sequence=CallbackSequence
                                                 , type=Type
                                                 }.

get_variable_type(#{ <<"var_type">> := Type }) when Type =/= null ->
    { <<"variable">>, Type };
get_variable_type(_) ->
    <<"variable">>.

answer_advice_taken(AdviceTaken, MessageId, Pid) ->
    Pid ! {{ automate_service_port_engine, advice_taken}, MessageId, AdviceTaken }.

request_icon(Pid) ->
    Pid ! {{ automate_service_port_engine, request_icon} }.

get_icon_path(ServicePortId) ->
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s", [automate_configuration:asset_directory("public/icons")
                                           , ServicePortId
                                           ]))).

write_icon(Data, ServicePortId) ->
    file:write_file(get_icon_path(ServicePortId), Data).

apply_advice(#{ <<"type">> := <<"ADVICE_SET">>
              , <<"value">> := Value
              }, BridgeId) ->
    lists:filtermap(fun ({Advice, Content}) ->
                            case apply_advice(Advice, Content, BridgeId) of
                                true ->
                                    {true, Advice};
                                false ->
                                    false
                            end
                    end, maps:to_list(Value)).

apply_advice(<<"NOTIFY_SIGNAL_LISTENERS">>, Content, BridgeId) ->
    ?BACKEND:set_notify_signal_listeners(Content, BridgeId),
    {ok, Listeners} = ?BACKEND:get_signal_listeners(Content, BridgeId),
    report_current_listeners(self(), Listeners),
    true;

apply_advice(Advice, _Content, _BridgeId) ->
    io:fwrite("[~p] Unknown advice: ~p~n", [self(), Advice]),
    false.

report_current_listeners(Reported, Listeners) ->
    lists:foreach(fun ({ Pid, Key, SubKey }) ->
                          Reported ! { automate_channel_engine, add_listener, { Pid, Key, SubKey } }
                  end, Listeners).
