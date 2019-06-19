%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_sessions_check).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_provided/2
        , options/2
        , is_authorized/2
        ]).
-export([to_json/2]).

-include("./records.hrl").

-record(check_seq, { username }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req
    , #check_seq{ username=undefined }}.

content_types_provided(Req, State) ->
    {[ {<<"application/json">>, to_json}
     ], Req, State}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
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
                            { true, Req1, #check_seq{username=Username} };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
-spec to_json(cowboy_req:req(), #check_seq{}) -> {binary(),cowboy_req:req(),_}.
to_json(Req, State) ->
    #check_seq{username=Username} = State,
    {ok, UserId} = automate_storage:get_userid_from_username(Username),

    Output = jiffy:encode(#{ <<"success">> => true
                           , <<"username">> => Username
                           , <<"user_id">> => UserId
                           }),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

    { Output, Res2, State }.
