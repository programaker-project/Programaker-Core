%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_users_root).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_accepted/2
        , options/2
        ]).
%% -export([is_authorized/2]).

-export([accept_json_modify_collection/2]).
%% -include("include/records.hrl").
-include("./records.hrl").


-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

%% -spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
%% is_authorized(Req, State) ->
%%     rest_is_authorized:is_authorized(Req, State).

%% CORS
options(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    {ok, Req2, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, accept_json_modify_collection}],
   Req, State}.

%%%% POST
%
-spec accept_json_modify_collection(cowboy_req:req(),#rest_session{}) -> {'true',cowboy_req:req(),_}.
accept_json_modify_collection(Req, Session) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, Req2} = read_body(Req),
            io:fwrite("--->~p ~n", [Body]),
            io:fwrite("-+->~p ~n", [jiffy:decode(Body, [return_maps])]),
            {true, Req2, Session};
        false ->
            {false, Req, Session }
    end.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
