%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_services_register).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).

-export([ accept_json_register_service/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").

-record(state, { username, service_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    ServiceId = cowboy_req:binding(service_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ username=UserId
            , service_id=ServiceId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    #state{username=Username} = State,
                    case automate_rest_api_backend:is_valid_token(X) of
                        {true, Username} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching username
                            { { false, <<"Unauthorized to register a service here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.


%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []},
       accept_json_register_service}],
     Req, State}.

-spec accept_json_register_service(cowboy_req:req(),
                                   #state{}) -> {true, cowboy_req:req(), #state{}}.
accept_json_register_service(Req, State) ->
    #state{username = Username, service_id = ServiceId} = State,
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    FullRegistrationData = jiffy:decode(Body, [return_maps]),
    { RegistrationData, ConnectionId } = case FullRegistrationData of
                                             #{ <<"metadata">> := #{<<"connection_id">> := ConnId} } ->
                                                 {maps:remove(<<"metadata">>, FullRegistrationData), ConnId};
                                             #{ <<"metadata">> := #{} } ->
                                                 {maps:remove(<<"metadata">>, FullRegistrationData), undefined};
                                             _ ->
                                                 {FullRegistrationData, undefined}
                                         end,
    case automate_rest_api_backend:register_service(Username, ServiceId, RegistrationData, ConnectionId) of
        {ok, Data} ->
            Output = jiffy:encode(Data),
            Res2 = ?UTILS:send_json_output(Output, Req1),
            {true, Res2, State}
    end.
