%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_programs_specific).
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

-record(get_program_seq, { username, program_name }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ProgramName = cowboy_req:binding(program_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #get_program_seq{ username=UserId
                      , program_name=ProgramName
                      }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#get_program_seq{username=Username}) ->
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
                    case automate_rest_api_backend:is_valid_token(X) of
                        {true, Username} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching username
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% Get handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #get_program_seq{})
             -> { stop | binary() ,cowboy_req:req(), #get_program_seq{}}.
to_json(Req, State) ->
    #get_program_seq{username=Username, program_name=ProgramName} = State,
    case automate_rest_api_backend:get_program(Username, ProgramName) of
        { ok, Program=#user_program{ id=ProgramId } } ->

            Checkpoint = case automate_storage:get_last_checkpoint_content(ProgramId) of
                             {ok, Content } ->
                                 Content;
                             {error, not_found} ->
                                 null
                         end,
            Output = program_to_json(Program, Checkpoint),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State };
        {error, Reason} ->
            Code = 500,
            Output = jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }),
            Res = cowboy_req:reply(Code, #{ <<"content-type">> => <<"application/json">> }, Output, Req),
            { stop, Res, State }
    end.


program_to_json(#user_program{ id=Id
                             , owner=Owner=#{ id := OwnerId}
                             , program_name=ProgramName
                             , program_type=ProgramType
                             , program_parsed=ProgramParsed
                             , program_orig=ProgramOrig
                             , enabled=Enabled
                             },
                Checkpoint) ->

    jiffy:encode(#{ <<"id">> => Id
                  , <<"owner">> => OwnerId
                  , <<"owner_data">> => Owner
                  , <<"name">> => ProgramName
                  , <<"type">> => ProgramType
                  , <<"parsed">> => ProgramParsed
                  , <<"orig">> => ProgramOrig
                  , <<"enabled">> => Enabled
                  , <<"checkpoint">> => Checkpoint
                  }).


content_types_accepted(Req, State) ->
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
update_program(Req, State) ->
    #get_program_seq{program_name=ProgramName, username=Username} = State,

    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = [jiffy:decode(Body, [return_maps])],
    Program = decode_program(Parsed),
    case automate_rest_api_backend:update_program(Username, ProgramName, Program) of
        ok ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% PATCH handler
update_program_metadata(Req, State) ->
    #get_program_seq{program_name=ProgramName, username=Username} = State,

    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = [jiffy:decode(Body, [return_maps])],
    Metadata = decode_program_metadata(Parsed),
    case automate_rest_api_backend:update_program_metadata(Username, ProgramName, Metadata) of
        {ok, #{ <<"link">> := Link } } ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => true, <<"link">> => Link}), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% DELETE handler
delete_resource(Req, State) ->
    #get_program_seq{program_name=ProgramName, username=Username} = State,
    case automate_rest_api_backend:delete_program(Username, ProgramName) of
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
