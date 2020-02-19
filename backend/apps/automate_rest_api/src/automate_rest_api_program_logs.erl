%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_logs).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , resource_exists/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").

-record(state, { user_id, program_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #state{ user_id=UserId
                       , program_id=ProgramId
                       }}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            { false, Req, State };
        _ ->
            { true, Req, State}
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("[Program/Logs] Asking for methods~n", []),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        _ ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    #state{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching user_id
                            { { false, <<"Non-matching user id">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{ program_id=ProgramId} = State,
    case automate_rest_api_backend:get_program_logs(ProgramId) of
        { ok, Logs } ->
            Output = serialize_logs(Logs),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


serialize_logs(Logs) ->
    jiffy:encode(lists:map(fun (Entry) -> serialize_log_entry(Entry) end, Logs)).

serialize_log_entry(#user_program_log_entry{ program_id=ProgramId
                                           , thread_id=ThreadId
                                           , user_id=UserId
                                           , block_id=BlockId
                                           , event_data=EventData
                                           , event_message=EventMessage
                                           , event_time=EventTime
                                           , severity=Severity
                                           , exception_data=_ExceptionData
                                           }) ->
    #{ program_id => ProgramId
     , thread_id => serialize_string_or_none(ThreadId)
     , user_id => serialize_string_or_none(UserId)
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
