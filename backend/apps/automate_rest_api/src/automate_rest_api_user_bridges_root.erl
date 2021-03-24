%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_user_bridges_root).

-export([init/2]).

-export([ allowed_methods/2
        , content_types_accepted/2
        , is_authorized/2
        , options/2
        , resource_exists/2
        , content_types_provided/2
        ]).

-export([ accept_json_create_service_port/2
        , to_json/2
        ]).

-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-define(URLS, automate_rest_api_utils_urls).

-record(state, {user_id:: binary()}).

-spec init(_, _) -> {cowboy_rest, _, _}.

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req,
     #state{user_id=UserId}}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> -> {false, Req, State};
        _ -> {true, Req, State}
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(), _) -> {[binary()], cowboy_req:req(), _}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{user_id=UserId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> -> {true, Req1, State};
        Method ->
            case cowboy_req:header(<<"authorization">>, Req,
                                   undefined)
            of
                undefined ->
                    {{false, <<"Authorization header not found">>}, Req1,
                     State};
                X ->
                    Scope = case Method of
                                <<"GET">> -> list_bridges;
                                <<"POST">> -> create_bridges
                            end,
                    case automate_rest_api_backend:is_valid_token_uid(X, Scope) of
                        {true, UserId} -> {true, Req1, State};
                        {true, _} -> %% Non matching UserId
                            {{false, <<"Unauthorized">>},
                             Req1, State};
                        false ->
                            {{false, <<"Authorization not correct">>}, Req1, State}
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{user_id=UserId}) ->
    case automate_service_port_engine:get_user_service_ports({user, UserId}) of
        { ok, Bridges } ->
            Output = jiffy:encode(lists:map(fun ?FORMATTING:bridge_to_json/1, Bridges)),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []},
       accept_json_create_service_port}],
     Req, State}.

-spec accept_json_create_service_port(cowboy_req:req(),
                                      #state{}) -> {{true,
                                                     binary()},
                                                    cowboy_req:req(),
                                                    #state{}}.

accept_json_create_service_port(Req, State=#state{user_id=UserId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    #{ <<"name">> := ServicePortName } = jiffy:decode(Body, [return_maps]),

    case automate_service_port_engine:create_service_port({user, UserId}, ServicePortName)  of
        {ok, ServicePortId} ->
            ServicePortUrl = ?URLS:bridge_control_url(ServicePortId),
            Output = jiffy:encode(#{ control_url => ServicePortUrl
                                   , id => ServicePortId
                                   }),
            Res2 = cowboy_req:set_resp_body(Output, Req1),
            Res3 = cowboy_req:delete_resp_header(<<"content-type">>,
                                                 Res2),
            Res4 = cowboy_req:set_resp_header(<<"content-type">>,
                                              <<"application/json">>, Res3),
            {{true, ServicePortUrl}, Res4, State}
    end.
