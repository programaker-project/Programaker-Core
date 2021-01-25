-module(automate_rest_api_utils_formatting).

-export([ format_message/1
        , serialize_logs/1
        , serialize_log_entry/1
        , serialize_icon/1
        , serialize_maybe_undefined/1
        , reason_to_json/1
        , group_to_json/1
        , group_and_role_to_json/1
        , program_listing_to_json/1
        , program_listing_to_json/2
        , program_data_to_json/2
        , collaborator_to_json/1
        , bridge_to_json/1
        , connection_to_json/1
        , asset_list_to_json/1
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-define(UTILS, automate_rest_api_utils).

format_message(Log=#user_program_log_entry{}) ->
    {ok, #{ type => program_log
          , value => serialize_log_entry(Log)
          }};
format_message(_) ->
    {error, unknown_format}.


serialize_logs(Logs) ->
    lists:map(fun (Entry) -> serialize_log_entry(Entry) end, Logs).

serialize_log_entry(#user_program_log_entry{ program_id=ProgramId
                                           , thread_id=ThreadId
                                           , owner=Owner
                                           , block_id=BlockId
                                           , event_data=EventData
                                           , event_message=EventMessage
                                           , event_time=EventTime
                                           , severity=Severity
                                           , exception_data=_ExceptionData
                                           }) ->
    {OwnerType, OwnerId} = case Owner of
                               { Type, Id } -> {Type, Id};
                               Id ->
                                   automate_logging:log_platform(migration_warning,
                                                                 io_lib:format("Unfinished migration. Found bare user Id on program: ~p", [Id])),
                                   { user, Id }
                           end,

    #{ program_id => ProgramId
     , thread_id => serialize_string_or_none(ThreadId)
     , owner => #{ type => OwnerType, id => serialize_string_or_none(OwnerId) }
     , user_id => serialize_string_or_none(OwnerId)
     , block_id => serialize_string_or_none(BlockId)
     , event_data => serialize_event_error(EventData)
     , event_message => EventMessage
     , event_time => EventTime
     , severity => Severity
     }.

serialize_string_or_none(none) ->
    null;
serialize_string_or_none(String) ->
    String.

serialize_event_error(#program_error{ error=Error
                                    , block_id=BlockId
                                    }) ->
    #{ error => serialize_error_subtype(Error)
     , block_id => BlockId
     };
serialize_event_error(_) ->
    unknown_error.

-spec serialize_error_subtype(program_error_type()) -> map().
serialize_error_subtype(#variable_not_set{variable_name=VariableName}) ->
    #{ type => variable_not_set
     , variable_name => VariableName
     };

serialize_error_subtype(#memory_not_set{block_id=BlockId}) ->
    #{ type => memory_not_set
     , block_id => BlockId
     };

serialize_error_subtype(#list_not_set{list_name=ListName}) ->
    #{ type => list_not_set
     , list_name => ListName
     };

serialize_error_subtype(#index_not_in_list{ list_name=ListName
                                          , index=Index
                                          , max=MaxIndex
                                          }) ->
    #{ type => index_not_in_list
     , list_name => ListName
     , index => Index
     , length => MaxIndex
     };

serialize_error_subtype(#invalid_list_index_type{ list_name=ListName
                                                , index=Index
                                                }) ->
    #{ type => invalid_list_index_type
     , list_name => ListName
     , index => Index
     };

serialize_error_subtype(#disconnected_bridge{ bridge_id=BridgeId
                                            , action=Action
                                            }) ->
    #{ type => disconnected_bridge
     , bridge_id => BridgeId
     , action => Action
     };

serialize_error_subtype(#bridge_call_connection_not_found{ bridge_id=BridgeId
                                                         , action=Action
                                                         }) ->
    #{ type => bridge_call_connection_not_found
     , bridge_id => BridgeId
     , action => Action
     };

serialize_error_subtype(#bridge_call_timeout{ bridge_id=BridgeId
                                            , action=Action
                                            }) ->
    #{ type => bridge_call_timeout
     , bridge_id => BridgeId
     , action => Action
     };

serialize_error_subtype(#bridge_call_failed{ bridge_id=BridgeId
                                           , action=Action
                                           , reason=Reason
                                           }) ->
    #{ type => bridge_call_failed
     , bridge_id => BridgeId
     , action => Action
     , reason => serialize_maybe_undefined(Reason)
     };

serialize_error_subtype(#bridge_call_error_getting_resource{ bridge_id=BridgeId
                                                           , action=Action
                                                           }) ->
    #{ type => bridge_call_error_getting_resource
     , bridge_id => BridgeId
     , action => Action
     };

serialize_error_subtype(#unknown_operation{}) ->
    #{ type => unknown_operation
     }.


serialize_icon(undefined) ->
    null;
serialize_icon({url, Url}) ->
    #{ <<"url">> => Url };
serialize_icon({hash, HashType, HashResult}) ->
    #{ HashType => HashResult }.

serialize_maybe_undefined(undefined) ->
    null;
serialize_maybe_undefined(X) ->
    X.

%% Reason
reason_to_json({Type, Subtype}) ->
    #{ type => Type
     , subtype => Subtype
     };
reason_to_json(Type) ->
    #{ type => Type
     }.

group_to_json(#user_group_entry{ id=Id
                               , name=Name
                               , canonical_name=CanonicalName
                               , public=IsPublic
                               }) ->
    Picture = case ?UTILS:group_has_picture(Id) of
                  false -> null;
                  true ->
                      <<"/groups/by-id/", Id/binary, "/picture">>
              end,
    #{ id => Id
     , name => Name
     , public => IsPublic
     , canonical_name => CanonicalName
     , picture => Picture
     }.


group_and_role_to_json({Group, Role}) ->
    GroupJson = group_to_json(Group),
    GroupJson#{ role => Role }.

program_listing_to_json(#user_program_entry{ id=Id
                                           , program_name=Name
                                           , enabled=Enabled
                                           , program_type=Type
                                           }) ->
    #{ id => Id
     , name => Name
     , enabled => Enabled
     , type => Type
     };
program_listing_to_json(#program_metadata{ id=Id
                                         , name=Name
                                         , link=Link
                                         , enabled=Enabled
                                         , type=Type
                                         }) ->
    #{ id => Id
     , name => Name
     , link =>  Link
     , enabled => Enabled
     , type => Type
     }.

program_listing_to_json(Program, Bridges) ->
    Base = program_listing_to_json(Program),
    Base#{ bridges_in_use => Bridges }.


program_data_to_json(#user_program{ id=Id
                                  , owner=Owner=#{ id := OwnerId}
                                  , program_name=ProgramName
                                  , program_type=ProgramType
                                  , program_parsed=ProgramParsed
                                  , program_orig=ProgramOrig
                                  , enabled=Enabled
                                  , is_public=IsPublic
                                  },
                     Checkpoint) ->
    #{ <<"id">> => Id
     , <<"owner">> => OwnerId
     , <<"owner_full">> => Owner
     , <<"name">> => ProgramName
     , <<"type">> => ProgramType
     , <<"parsed">> => ProgramParsed
     , <<"orig">> => ProgramOrig
     , <<"enabled">> => Enabled
     , <<"checkpoint">> => Checkpoint
     , <<"is_public">> => IsPublic
     }.


collaborator_to_json({ #registered_user_entry{ id=Id
                                             , username=Username
                                             }
                     , Role
                     }) ->
    Picture = case ?UTILS:user_has_picture(Id) of
                  false -> null;
                  true ->
                      <<"/users/by-id/", Id/binary, "/picture">>
              end,
    #{ id => Id
     , username => Username
     , role => Role
     , picture => Picture
     }.


bridge_to_json(#service_port_entry_extra{ id=Id
                                        , name=Name
                                        , owner={OwnerType, OwnerId}
                                        , is_connected=IsConnected
                                        , icon=Icon
                                        }) ->
    #{ <<"id">> => Id
     , <<"name">> => Name
     , <<"owner">> => OwnerId
     , <<"owner_full">> => #{type => OwnerType, id => OwnerId}
     , <<"is_connected">> => IsConnected
     , <<"icon">> => serialize_icon(Icon)
     }.

-spec connection_to_json(#user_to_bridge_connection_entry{}) -> false | {true, map()}.
connection_to_json(#user_to_bridge_connection_entry{ id=Id
                                                   , bridge_id=BridgeId
                                                   , owner=_
                                                   , channel_id=_
                                                   , name=Name
                                                   , creation_time=_CreationTime
                                                   , save_signals=Saving
                                                   }) ->
    case automate_service_port_engine:get_bridge_info(BridgeId) of
        {ok, #service_port_metadata{ name=BridgeName, icon=Icon }} ->
            {true, #{ <<"connection_id">> => Id
                    , <<"name">> => serialize_string_or_undefined(Name)
                    , <<"bridge_id">> => BridgeId
                    , <<"bridge_name">> => serialize_string_or_undefined(BridgeName)
                    , <<"icon">> => serialize_icon(Icon)
                    , <<"saving">> => Saving
                    } };
        {error, _Reason} ->
            false
    end.

-spec asset_list_to_json([#user_asset_entry{}]) -> [map()].
asset_list_to_json(Assets) ->
    lists:map(fun(#user_asset_entry{ asset_id={ {OwnerType, OwnerId}, AssetId }, mime_type=MimeType }) ->
                      #{ id => AssetId
                       , owner_full => #{ type => OwnerType
                                        , id => OwnerId
                                        }
                       , mime_type => case MimeType of
                                          { Type, undefined } ->
                                              Type;
                                          { Type, Subtype } ->
                                              <<Type/binary, "/", Subtype/binary>>
                                      end
                       }
              end, Assets).

serialize_string_or_undefined(undefined) ->
    null;
serialize_string_or_undefined(String) ->
    String.
