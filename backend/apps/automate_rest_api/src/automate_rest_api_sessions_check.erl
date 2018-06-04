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

-record(login_seq, { rest_session,
                     login_data
                   }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req
    , #login_seq{ rest_session=undefined
                , login_data=undefined}}.

content_types_provided(Req, State) ->
    {[ {<<"application/json">>, to_json}
     ], Req, State}.

%% CORS
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"Access-Control-Max-Age">>, <<"3600">>, Req2),
    Req4 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>,
                                      <<"authorization, content-type, xsrf-token">>, Req3),
    Req5 = cowboy_req:set_resp_header(<<"Access-Control-Expose-Headers">>,
                                      <<"xsrf-token">>, Req4),
    {ok, Req5, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req, undefined) of
        undefined ->
            { {false, <<"Authorization header not found">>} , Req, State };
        X ->
            case automate_rest_api_backend:is_valid_token(X) of
                true ->
                    { true, Req, State };
                false ->
                    { { false, <<"Authorization not correct">>}, Req, State }
            end
    end.

%% GET handler
-spec to_json(cowboy_req:req(), #rest_session{}) -> {binary(),cowboy_req:req(),_}.
to_json(Req, State) ->
    Output = jiffy:encode(#{ <<"success">> => true }),
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
    { true, Res3, State }.
