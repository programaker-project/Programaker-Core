-module(automate_rest_api_program_assets_root).
-export([ init/2
        , allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).
-export([ accept_file/2
        ]).

-include("./records.hrl").
-include("../../automate_common_types/src/types.hrl").
-include("../../automate_storage/src/records.hrl").

-define(UTILS, automate_rest_api_utils).

-record(state, { owner_id :: owner_id() | undefined
               , program_id :: binary()
               , copy_from :: undefined | { binary(), binary() }
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    ProgramId = cowboy_req:binding(program_id, Req1),
    Qs = cowboy_req:parse_qs(Req),
    CopyFrom = case proplists:get_value(<<"copy_from">>, Qs, undefined) of
                   undefined -> undefined;
                   From when is_binary(From) ->
                       [FromProgramId, FromAssetId] = binary:split(From, <<"/">>),
                       {FromProgramId, FromAssetId}
               end,

    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , owner_id=undefined
            , copy_from=CopyFrom
            }}.


%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId, copy_from=CopyFrom}) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X, create_assets) of
                        {true, UId} ->
                            {ok, Owner} = automate_storage:get_program_owner(ProgramId),
                            case automate_storage:can_user_edit_as({user, UId}, Owner) of
                                true ->
                                    case CopyFrom of
                                        undefined ->
                                            { true, Req1, State#state{owner_id=Owner} };
                                        {FromProgram, _} ->
                                            case automate_storage:is_user_allowed({user, UId}, FromProgram, read_program) of
                                                {ok, true} ->
                                                    { true, Req1, State#state{owner_id=Owner} };
                                                {ok, false} ->
                                                    { { false, <<"Cannot copy from source program">>}, Req1, State }
                                            end
                                    end;
                                false ->
                                    { { false, <<"Action not authorized">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"multipart">>, <<"form-data">>, []}, accept_file}],
     Req, State}.

-spec accept_file(cowboy_req:req(), #state{}) -> {boolean(),cowboy_req:req(), #state{}}.
accept_file(Req, State=#state{owner_id=OwnerId, copy_from={FromProgramId, FromAssetId}}) ->
    {ok, FromProgramOwner} = automate_storage:get_program_owner(FromProgramId),

    %% TODO: Implement REST check to return the appropriate HTTP code
    case FromProgramOwner == OwnerId of
        true ->
            ok;
        false ->
            case automate_storage:get_user_asset_info(OwnerId, FromAssetId) of
                {error, not_found} ->
                    {ok, #user_asset_entry{ mime_type=MimeType }} = automate_storage:get_user_asset_info(FromProgramOwner, FromAssetId),
                    ?UTILS:copy_asset(FromProgramOwner, OwnerId, FromAssetId),
                    ok = automate_storage:add_user_asset(OwnerId, FromAssetId, MimeType);
                {ok, _AssetInfo} ->
                    %% No need to do anything, already exists
                    ok
            end
    end,
    {true, Req, State};
accept_file(Req, State=#state{owner_id=OwnerId}) ->
    Path = ?UTILS:get_owner_asset_directory(OwnerId),
    {ok, {AssetId, FileType}, Req1} = ?UTILS:stream_body_to_file_hashname(Req, Path, <<"file">>),

    MimeType = case binary:split(FileType, <<"/">>) of
                   [Type, SubType] ->
                       {Type, SubType};
                   [Type] ->
                       {Type, undefined}
               end,
    ok = automate_storage:add_user_asset(OwnerId, AssetId, MimeType),

    Output = jiffy:encode(#{ success => true
                           , value => AssetId
                           }),
    Res2 = cowboy_req:set_resp_body(Output, Req1),
    Res3 = cowboy_req:delete_resp_header(<<"content-type">>,
                                         Res2),
    Res4 = cowboy_req:set_resp_header(<<"content-type">>,
                                      <<"application/json">>, Res3),
    {true, Res4, State}.
