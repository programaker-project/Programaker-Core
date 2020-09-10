-module(automate_rest_api_utils_formatting).

-export([ format_message/1
        , serialize_logs/1
        , serialize_log_entry/1
        , serialize_icon/1
        , serialize_maybe_undefined/1
        , reason_to_json/1
        , group_to_json/1
        , program_to_json/1
        , program_to_json/2
        , collaborator_to_json/1
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").

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

serialize_error_subtype(#variable_not_set{variable_name=VariableName}) ->
    #{ type => variable_not_set
     , variable_name => VariableName
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
    #{ id => Id
     , name => Name
     , public => IsPublic
     , canonical_name => CanonicalName
     }.

program_to_json(#user_program_entry{ id=Id
                                   , program_name=Name
                                   , enabled=Enabled
                                   , program_type=Type
                                   }) ->
    #{ id => Id
     , name => Name
     , enabled => Enabled
     , type => Type
     };
program_to_json(#program_metadata{ id=Id
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

program_to_json(Program, Bridges) ->
    Base = program_to_json(Program),
    Base#{ bridges_in_use => Bridges }.

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
