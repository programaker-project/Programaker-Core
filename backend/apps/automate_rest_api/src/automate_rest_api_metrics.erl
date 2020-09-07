%%% @doc
%%% REST endpoint to work as prometheus exporter.
%%% @end

-module(automate_rest_api_metrics).
-export([init/2]).
-export([ content_types_provided/2
        , is_authorized/2
        ]).

-export([ to_text/2
        ]).

-include("./records.hrl").
-define(APPLICATION, automate_rest_api).
-define(METRICS_BEARER_TOKEN_SETTING, metrics_secret).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req, { }}.


%% Authorization
is_authorized(Req, State) ->
    case application:get_env(?APPLICATION, ?METRICS_BEARER_TOKEN_SETTING) of
        %% No setting, we allow anything
        undefined ->
            { true, Req, State };
        {ok, Secret} ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req, State };
                <<"Bearer ", Secret/binary>> ->
                    { true, Req, State };
                X ->
                    { { false, <<"Authorization not correct">>}, Req, State }
            end
    end.


%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"*">>, <<"*">>, []}, to_text}],
     Req, State}.

-spec to_text(cowboy_req:req(), {})
             -> {binary(),cowboy_req:req(), {}}.

to_text(Req, State) ->
    Output = automate_stats:format(prometheus),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Res1),
    { Output, Res2, State }.
