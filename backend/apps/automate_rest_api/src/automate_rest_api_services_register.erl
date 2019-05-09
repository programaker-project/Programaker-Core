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
    io:fwrite("[Service] Asking for methods~n", []),
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
    {ok, Body, Req1} = read_body(Req),
    RegistrationData = jiffy:decode(Body, [return_maps]),

    case automate_rest_api_backend:register_service(Username, ServiceId, RegistrationData) of
        {ok, Data} ->
            Output = jiffy:encode(Data),
            Res2 = cowboy_req:set_resp_body(Output, Req1),
            Res3 = cowboy_req:delete_resp_header(<<"content-type">>,
                                                 Res2),
            Res4 = cowboy_req:set_resp_header(<<"content-type">>,
                                              <<"application/json">>, Res3),
            {true, Res4, State}
    end.

read_body(Req0) -> read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.
