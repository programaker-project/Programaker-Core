%%% @doc
%%% REST endpoint to work as prometheus exporter.
%%% @end

-module(automate_rest_api_metrics).
-export([init/2]).
-export([ content_types_provided/2
        ]).

-export([ to_text/2
        , prepare/0
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
    collect(),
    Output = prometheus_text_format:format(),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Res1),
    { Output, Res2, State }.


%%====================================================================
%% Data preparation and control
%%====================================================================
collect() ->
    Services = [ automate_storage_sup

               , automate_channel_engine
               , automate_channel_engine_sup

               , automate_rest_api_sup

               , automate_service_registry_sup

               , automate_bot_engine_runner_sup
               , automate_bot_engine_sup

               , automate_monitor_engine_runner_sup
               , automate_monitor_engine_sup

               , automate_services_telegram_demux
               , automate_services_telegram_sup
               ],

    lists:foreach(fun (S) ->
                          prometheus_boolean:set(automate_service, [S],
                                                 whereis(S) =/= undefined)
                  end, Services).

prepare() ->
    prometheus_boolean:new([{name, automate_service}, {labels, [name]}, {help, "State of automate service."}]),

    ok.
