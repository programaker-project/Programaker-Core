%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_service_ports_root).

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

-record(state, {username}).

-spec init(_, _) -> {cowboy_rest, _, _}.

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req,
     #state{username = UserId}}.

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
    io:fwrite("Asking for methods~n", []),
    {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> -> {true, Req1, State};
        _ ->
            case cowboy_req:header(<<"authorization">>, Req,
                                   undefined)
            of
                undefined ->
                    {{false, <<"Authorization header not found">>}, Req1,
                     State};
                X ->
                    #state{username = Username} = State,
                    case automate_rest_api_backend:is_valid_token(X) of
                        {true, Username} -> {true, Req1, State};
                        {true, _} -> %% Non matching username
                            {{false, <<"Unauthorized to create a program here">>},
                             Req1, State};
                        false ->
                            {{false, <<"Authorization not correct">>}, Req1, State}
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    io:fwrite("User > Bridge > ID~n", []),
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{username=Username} = State,
    case automate_rest_api_backend:list_bridges(Username) of
        { ok, Bridges } ->
            Output = jiffy:encode(lists:map(fun to_map/1, Bridges)),

            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.

to_map(#service_port_entry_extra{ id=Id
                                , name=Name
                                , owner=Owner
                                , service_id=ServiceId
                                , is_connected=IsConnected
                                }) ->
    #{ <<"id">> => Id
     , <<"name">> => Name
     , <<"owner">> => Owner
     , <<"service_id">> => ServiceId
     , <<"is_connected">> => IsConnected
     }.

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

accept_json_create_service_port(Req, State) ->
    #state{username = Username} = State,
    {ok, Body, Req1} = read_body(Req),
    #{ <<"name">> := ServicePortName } = jiffy:decode(Body, [return_maps]),

    case automate_rest_api_backend:create_service_port(Username, ServicePortName) of
        {ok, ServicePortUrl} ->
            Output = jiffy:encode(#{<<"control_url">> => ServicePortUrl}),
            Res2 = cowboy_req:set_resp_body(Output, Req1),
            Res3 = cowboy_req:delete_resp_header(<<"content-type">>,
                                                 Res2),
            Res4 = cowboy_req:set_resp_header(<<"content-type">>,
                                              <<"application/json">>, Res3),
            {{true, ServicePortUrl}, Res4, State}
    end.

read_body(Req0) -> read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.
