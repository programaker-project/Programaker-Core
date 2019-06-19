%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_service_port_oauth_return).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").

-record(state, { service_port_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ServicePortId = cowboy_req:binding(service_port_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ service_port_id=ServicePortId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("[SPService] Returning OAuth~n", []),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

%% GET handler
content_types_provided(Req, State) ->
    io:fwrite("User > service-port > oauth-return~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{service_port_id=ServicePortId} = State,
    Qs = cowboy_req:qs(Req),
    case automate_rest_api_backend:send_oauth_return(ServicePortId, Qs) of
        ok ->
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { jiffy:encode(#{ <<"success">> => true }), Res2, State };
        {error, Reason} ->

            Code = case Reason of
                       not_found -> 404;
                       unauthorized -> 403;
                       _ -> 500
                   end,

            cowboy_req:reply(Code,
                             #{ <<"content-type">> => <<"application/json">> },
                             jiffy:encode(#{ <<"success">> => false
                                           , <<"message">> => Reason
                                           }),
                             Req)
    end.

