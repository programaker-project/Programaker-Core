-module(automate_rest_api_program_assets_by_id).
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
-include("../../automate_common_types/src/types.hrl").

-define(UTILS, automate_rest_api_utils).

-record(state, { owner_id :: owner_id() | undefined
               , program_id :: binary()
               , asset_id :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    ProgramId = cowboy_req:binding(program_id, Req1),
    AssetId = cowboy_req:binding(asset_id, Req1),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , asset_id=AssetId
            , owner_id=undefined
            }}.

resource_exists(Req, State=#state{program_id=ProgramId, asset_id=AssetId}) ->
    {ok, Owner} = automate_storage:get_program_owner(ProgramId),
    io:fwrite("Looking for  ~p~n", [{Owner, AssetId}]),
    {?UTILS:owner_has_asset(Owner, AssetId), Req, State#state{owner_id=Owner}}.


%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{owner_id=_OwnerId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        <<"GET">> ->
            { true, Req1, State }
    end.


%% Image handler
content_types_provided(Req, State) ->
    {[{{<<"octet">>, <<"stream">>, []}, retrieve_file}],
     Req, State}.

-spec retrieve_file(cowboy_req:req(), #state{}) -> {stop | boolean(),cowboy_req:req(), #state{}}.
retrieve_file(Req, State=#state{asset_id=AssetId, owner_id=Owner}) ->
    Dir = ?UTILS:get_owner_asset_directory(Owner),
    Path = list_to_binary([Dir, "/", AssetId]),
    FileSize = filelib:file_size(Path),

    Res = cowboy_req:reply(200, #{ %% <<"content-type">> => "image/png"
                                 }, {sendfile, 0, FileSize, Path}, Req),
    {stop, Res, State}.
