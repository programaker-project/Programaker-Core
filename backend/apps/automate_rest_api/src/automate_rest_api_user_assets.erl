%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_user_assets).
-export([ init/2
        , allowed_methods/2
        , content_types_provided/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).
-export([ accept_file/2
        , list_files_to_json/2
        ]).

-include("../../automate_storage/src/records.hrl").
-include("./records.hrl").
-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).

-record(state, { owner_id :: owner_id() | undefined
               }).

-spec init(_, [user | group]) -> {'cowboy_rest',_,_}.
init(Req, [OwnerType]) ->
    OwnerId = case OwnerType of
                  user ->
                      {user, cowboy_req:binding(user_id, Req)};
                  group ->
                      {group, cowboy_req:binding(group_id, Req)}
              end,
    {cowboy_rest, Req
    , #state{ owner_id=OwnerId
            }}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{owner_id=OwnerId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        _ ->
            Action = case cowboy_req:method(Req1) of
                         <<"GET">> -> can_user_view_as;
                         <<"DELETE">> -> can_user_admin_as;
                         _ -> can_user_edit_as
                     end,
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            case automate_storage:Action({user, UserId}, OwnerId) of
                                true ->
                                    { true, Req1, State };
                                false ->
                                    { { false, <<"Action not authorized">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Unauthorized">>}, Req1, State }
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

%% Image handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, list_files_to_json}],
     Req, State}.

-spec list_files_to_json(cowboy_req:req(), #state{}) -> {binary(),cowboy_req:req(), #state{}}.
list_files_to_json(Req, State=#state{owner_id=OwnerId}) ->
    {ok, Assets} = automate_storage:list_user_assets(OwnerId),

    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

    { jiffy:encode(#{ success => true
                    , assets => ?FORMATTING:asset_list_to_json(Assets)
                    }), Res2, State }.
