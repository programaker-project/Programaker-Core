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

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).

-record(state, { program_id :: binary(), user_id :: binary() | undefined }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , user_id=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };

        %% Reading a public program
        Method ->
            {ok, #user_program_entry{ visibility=Visibility }} = automate_storage:get_program_from_id(ProgramId),
            IsPublic = ?UTILS:is_public(Visibility),
            {Action, Scope} = case Method of
                                  <<"GET">> -> {read_program, { read_program, ProgramId }};
                                  <<"PUT">> -> {edit_program, { edit_program, ProgramId }};
                                  <<"PATCH">> -> {edit_program, { edit_program_metadata, ProgramId }};
                                  <<"DELETE">> -> {delete_program, { delete_program, ProgramId }}
                              end,
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    case {Method, IsPublic} of
                        {<<"GET">>, true} ->
                            { true, Req1, State };
                        _ ->
                            { {false, <<"Authorization header not found">>} , Req1, State }
                    end;
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X, Scope) of
                        {true, UId} ->
                            case automate_storage:is_user_allowed({user, UId}, ProgramId, Action) of
                                {ok, true} ->
                                    { true, Req1, State#state{user_id=UId} };
                                {ok, false} ->
                                    case {Method, IsPublic} of
                                        {<<"GET">>, true} ->
                                            {true, Req1, State#state{user_id=UId}};
                                        _ ->
                                            { { false, <<"Action not authorized">>}, Req1, State }
                                    end;
                                {error, Reason} ->
                                    automate_logging:log_api(warning, ?MODULE, {authorization_error, Reason}),
                                    { { false, <<"Error on authorization">>}, Req1, State }
                            end;
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
to_json(Req, State=#state{program_id=ProgramId, user_id=UserId}) ->
    Qs = cowboy_req:parse_qs(Req),
    IncludePages = case proplists:get_value(<<"retrieve_pages">>, Qs) of
                       <<"yes">> ->
                           true;
                       _ ->
                           false
               end,

    case automate_rest_api_backend:get_program(ProgramId) of
        { ok, Program=#user_program{last_upload_time=ProgramTime} } ->
            Checkpoint = case automate_storage:get_last_checkpoint_content(ProgramId) of
                             {ok, #user_program_checkpoint{event_time=CheckpointTime, content=Content} } ->
                                 case ProgramTime < (CheckpointTime / 1000) of
                                     true ->
                                         Content;
                                     false ->
                                         null
                                 end;
                             {error, not_found} ->
                                 null
                         end,

            Json = ?FORMATTING:program_data_to_json(Program, Checkpoint),

            {ok, CanEdit} = automate_storage:is_user_allowed({user, UserId}, ProgramId, edit_program),
            {ok, CanAdmin } = automate_storage:is_user_allowed({user, UserId}, ProgramId, admin_program),

            Json2 = Json#{ readonly => not CanEdit, can_admin => CanAdmin },
            Json3 = case IncludePages of
                        false -> Json2;
                        true ->
                            {ok, Pages} = automate_storage:get_program_pages(ProgramId),
                            Json#{ pages => maps:from_list(lists:map(fun (#program_pages_entry{ page_id={_, Path}
                                                                                              , contents=Contents}) ->
                                                                             {Path, Contents}
                                                                     end, Pages))
                                 }
                    end,

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { jiffy:encode(Json3), Res2, State }
    end.
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
update_program(Req, State=#state{program_id=ProgramId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    Program = decode_program(Parsed),
    case automate_rest_api_backend:update_program_by_id(ProgramId, Program) of
        ok ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% PATCH handler
update_program_metadata(Req, State=#state{program_id=ProgramId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    case automate_rest_api_backend:update_program_metadata(ProgramId, Parsed) of
        ok ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
            { true, Req2, State };
        { error, Reason } ->
            Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
            { false, Req2, State }
    end.

%% DELETE handler
delete_resource(Req, State=#state{program_id=ProgramId}) ->
    case automate_rest_api_backend:delete_program(ProgramId) of
        ok ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req),
            { false, Req1, State }
    end.


%% Converters
decode_program(P=#{ <<"type">> := ProgramType
                  , <<"orig">> := ProgramOrig
                  , <<"parsed">> := ProgramParsed
                  }) ->
    #program_content { type=ProgramType
                     , orig=ProgramOrig
                     , parsed=ProgramParsed
                     , pages=case P of
                                 #{ <<"pages">> := Pags } -> Pags;
                                 _ -> #{}
                             end
                     }.
