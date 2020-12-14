%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_render).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).

-record(state, { program_id :: binary()
               , path :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    Path = build_page_path(cowboy_req:path_info(Req)),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , path=Path
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

is_authorized(Req, State=#state{program_id=_ProgramId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    %% TODO: Require authentication?
    {true, Req1, State}.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"html">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {iolist(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{program_id=ProgramId, path=Path}) ->
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/html">>, Res1),

    {ok, #program_pages_entry{ contents=Contents }} = automate_storage:get_program_page(ProgramId, Path),

    { automate_rest_api_renderer:render_page(ProgramId, Contents), Res2, State }.


build_page_path(Path) ->
    list_to_binary(["/"] ++ lists:join("/", Path)).
