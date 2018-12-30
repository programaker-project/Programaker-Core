%%% @doc
%%% REST endpoint to work as prometheus exporter.
%%% @end

-module(automate_rest_api_metrics).
-export([init/2]).
-export([ content_types_provided/2
        ]).

-export([ to_text/2
        ]).

-include("./records.hrl").

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req, { }}.


%% GET handler
content_types_provided(Req, State) ->
    io:fwrite("Control types provided~n", []),
    {[{{<<"*">>, <<"*">>, []}, to_text}],
     Req, State}.

-spec to_text(cowboy_req:req(), {})
             -> {binary(),cowboy_req:req(), {}}.
to_text(Req, State) ->
    Output = prometheus_text_format:format(),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Res1),
    { Output, Res2, State }.
