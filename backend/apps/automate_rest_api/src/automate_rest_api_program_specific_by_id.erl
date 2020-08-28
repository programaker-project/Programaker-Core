%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_specific_by_id).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , delete_resource/2
        ]).

-export([ to_json/2
        , accept_json_program/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { program_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
                      }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("[SPProgram]Asking for methods~n", []),
    {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        _ ->
            Action = case cowboy_req:method(Req1) of
                         <<"GET">> -> read_program;
                         <<"DELETE">> -> delete_program;
                         _ -> edit_program
                     end,
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UId} ->
                            case automate_storage:is_user_allowed(UId, ProgramId, Action) of
                                {ok, true} ->
                                    { true, Req1, State };
                                {ok, false} ->
                                    { { false, <<"Action not authorized">>}, Req1, State };
                                {error, Reason} ->
                                    automate_logging:log_api(warn, ?MODULE, {authorization_error, Reason}),
                                    { { false, <<"Error on authorization">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% Get handler
content_types_provided(Req, State) ->
    io:fwrite("User > program > ID~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{program_id=ProgramId}) ->
    case automate_rest_api_backend:get_program(ProgramId) of
        { ok, Program } ->
            io:fwrite("PROGRAM: ~p~n", [Program]),
            Output = program_to_json(Program),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


program_to_json(#user_program{ id=Id
                             , owner=Owner=#{ id := OwnerId }
                             , program_name=ProgramName
                             , program_type=ProgramType
                             , program_parsed=ProgramParsed
                             , program_orig=ProgramOrig
                             , enabled=Enabled
                             }) ->

    jiffy:encode(#{ <<"id">> => Id
                  , <<"owner">> => OwnerId
                  , <<"owner_data">> => Owner
                  , <<"name">> => ProgramName
                  , <<"type">> => ProgramType
                  , <<"parsed">> => ProgramParsed
                  , <<"orig">> => ProgramOrig
                  , <<"enabled">> => Enabled
                  }).


content_types_accepted(Req, State) ->
    io:fwrite("[PUT] User > program > ID~n", []),
    {[{{<<"application">>, <<"json">>, []}, accept_json_program}],
     Req, State}.

accept_json_program(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            update_program(Req, State);
        <<"PATCH">> ->
            update_program_metadata(Req, State)
    end.

%% PUT handler
update_program(Req, State=#state{program_id=ProgramId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = [jiffy:decode(Body, [return_maps])],
    Program = decode_program(Parsed),
    case automate_rest_api_backend:update_program_by_id(ProgramId, Program) of
        ok ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% PATCH handler
update_program_metadata(Req, State=#state{program_id=ProgramId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = [jiffy:decode(Body, [return_maps])],
    Metadata = decode_program_metadata(Parsed),
    case automate_rest_api_backend:update_program_metadata(ProgramId, Metadata) of
        ok ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% DELETE handler
delete_resource(Req, State=#state{program_id=ProgramId}) ->
    case automate_rest_api_backend:delete_program(ProgramId) of
        ok ->
            Req1 = send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req),
            { false, Req1, State }
    end.


%% Converters
decode_program_metadata([#{ <<"name">> := ProgramName
                          }]) ->
    #editable_user_program_metadata { program_name=ProgramName
                                    }.


decode_program([#{ <<"type">> := ProgramType
                 , <<"orig">> := ProgramOrig
                 , <<"parsed">> := ProgramParsed
                 }]) ->
    #program_content { type=ProgramType
                     , orig=ProgramOrig
                     , parsed=ProgramParsed
                     }.


send_json_output(Output, Req) ->
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2).
