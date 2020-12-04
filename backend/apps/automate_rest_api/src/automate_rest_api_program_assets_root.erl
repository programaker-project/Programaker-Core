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

-define(UTILS, automate_rest_api_utils).

-record(state, { owner_id :: owner_id() | undefined
               , program_id :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    ProgramId = cowboy_req:binding(program_id, Req1),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , owner_id=undefined
            }}.


%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId}) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UId} ->
                            {ok, Owner} = automate_storage:get_program_owner(ProgramId),
                            case automate_storage:can_user_edit_as({user, UId}, Owner) of
                                true ->
                                    { true, Req1, State#state{owner_id=Owner} };
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
