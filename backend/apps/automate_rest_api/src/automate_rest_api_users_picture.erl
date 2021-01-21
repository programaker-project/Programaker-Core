%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_users_picture).
-export([ init/2
        , allowed_methods/2
        , content_types_provided/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        , resource_exists/2
        , last_modified/2
        ]).
-export([ accept_file/2
        , retrieve_file/2
        ]).

-include("./records.hrl").
-define(UTILS, automate_rest_api_utils).

-record(state, { user_id :: binary()
               , last_modification_time :: undefined | calendar:datetime()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #state{ user_id=UserId
            , last_modification_time=undefined
            }}.

resource_exists(Req, State=#state{user_id=UserId}) ->
    case ?UTILS:user_picture_modification_time(UserId) of
        {error, not_found} ->
            {false, Req, State};
        { ok, ModTime }->
            {true, Req, State#state{ last_modification_time=ModTime }}
    end.

last_modified(Req, State=#state{last_modification_time=ModTime}) ->
    {ModTime, Req, State}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{user_id=UserId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        <<"GET">> ->
            { true, Req1, State };
        _ ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {_, false} ->
                            { { false, <<"Unauthorized">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"multipart">>, <<"form-data">>, []}, accept_file}],
     Req, State}.

-spec accept_file(cowboy_req:req(), #state{}) -> {boolean(),cowboy_req:req(), #state{}}.
accept_file(Req, State=#state{user_id=UserId}) ->
    Path = ?UTILS:user_picture_path(UserId),
    {ok, _Data, Req1} = ?UTILS:stream_body_to_file(Req, Path, <<"file">>),
    {true, Req1, State}.


%% Image handler
content_types_provided(Req, State) ->
    {[{{<<"octet">>, <<"stream">>, []}, retrieve_file}],
     Req, State}.

-spec retrieve_file(cowboy_req:req(), #state{}) -> {stop | boolean(),cowboy_req:req(), #state{}}.
retrieve_file(Req, State=#state{user_id=UserId}) ->
    Path = ?UTILS:user_picture_path(UserId),
    FileSize = filelib:file_size(Path),

    Res = cowboy_req:reply(200, #{ %% <<"content-type">> => "image/png"
                                 }, {sendfile, 0, FileSize, Path}, Req),
    {stop, Res, State}.
