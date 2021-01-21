-module(automate_rest_api_user_asset_by_id).
-export([ init/2
        , allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , resource_exists/2
        ]).
-export([ retrieve_file/2
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-define(UTILS, automate_rest_api_utils).
-define(MAX_AGE_IMMUTABLE_SECONDS, 31536000). %% Seconds in a year

-record(state, { owner_id :: owner_id() | undefined
               , asset_id :: binary()
               , asset_info :: #user_asset_entry{} | undefined
               }).

-spec init(_, [user | group]) -> {'cowboy_rest',_,_}.
init(Req, [OwnerType]) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    OwnerId = case OwnerType of
                  user ->
                      {user, cowboy_req:binding(user_id, Req)};
                  group ->
                      {group, cowboy_req:binding(group_id, Req)}
              end,
    AssetId = cowboy_req:binding(asset_id, Req1),
    {cowboy_rest, Req1
    , #state{ owner_id=OwnerId
            , asset_id=AssetId
            , asset_info=undefined
            }}.

resource_exists(Req, State=#state{owner_id=OwnerId, asset_id=AssetId}) ->
    case automate_storage:get_user_asset_info(OwnerId, AssetId) of
        {error, not_found} ->
            {false, Req, State};
        {ok, AssetInfo} ->
            {true, Req, State#state{asset_info=AssetInfo}}
    end.


%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{owner_id=_OwnerId}) ->
    case cowboy_req:method(Req) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req, State };
        <<"GET">> ->
            { true, Req, State }
    end.


%% Image handler
content_types_provided(Req, State) ->
    {[{{<<"octet">>, <<"stream">>, []}, retrieve_file}],
     Req, State}.

-spec retrieve_file(cowboy_req:req(), #state{}) -> {stop | boolean(),cowboy_req:req(), #state{}}.
retrieve_file(Req, State=#state{ asset_id=AssetId
                               , owner_id=Owner
                               , asset_info=#user_asset_entry{mime_type=MimeType}
                               }) ->
    Dir = ?UTILS:get_owner_asset_directory(Owner),
    Path = list_to_binary([Dir, "/", AssetId]),
    FileSize = filelib:file_size(Path),

    ContentType = case MimeType of
                      { Type, undefined } ->
                          Type;
                      { Type, SubType } ->
                          list_to_binary([Type, "/", SubType])
                  end,

    Res = cowboy_req:reply(200, #{ <<"content-type">> => ContentType
                                 , <<"cache-control">> => list_to_binary(io_lib:fwrite("public, max-age=~p, immutable", [?MAX_AGE_IMMUTABLE_SECONDS]))
                                 }, {sendfile, 0, FileSize, Path}, Req),
    {stop, Res, State}.
