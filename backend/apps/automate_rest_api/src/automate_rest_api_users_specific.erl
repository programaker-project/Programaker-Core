%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_users_specific).
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

-export([to_json/2]).
%% -include("include/records.hrl").
-include("./records.hrl").


-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(_,_) -> {[{binary(),'to_json'},...],_,_}.
content_types_provided(Req, State) ->
    {[ {<<"application/json">>, to_json}
     ], Req, State}.

%%%% GET
%% Knowledge collection item retrieval
-spec to_json(cowboy_req:req(), #rest_session{}) -> {binary(),cowboy_req:req(),_}.
to_json(Req, State) ->
    UserId = cowboy_req:binding(user_id, Req),
    {UserId, Req, State}.
