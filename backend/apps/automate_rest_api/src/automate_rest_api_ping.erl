%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_ping).
-export([init/2]).
-export([ content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req, { }}.


%% GET handler
content_types_provided(Req, State) ->
    io:fwrite("Control types provided~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), {})
             -> {binary(),cowboy_req:req(), {}}.
to_json(Req, State) ->
    Output = jiffy:encode(#{ success => true }),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),
    { Output, Res2, State }.
