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
        ]).

-export([accept_json_create_service_port/2]).

-include("./records.hrl").

-record(create_service_port_seq, {username}).

-spec init(_, _) -> {cowboy_rest, _, _}.

init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req,
     #create_service_port_seq{username = UserId}}.

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
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    #create_service_port_seq{username = Username} = State,
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

%% POST handler
content_types_accepted(Req, State) ->
    io:fwrite("Control types accepted~n", []),
    {[{{<<"application">>, <<"json">>, []},
       accept_json_create_service_port}],
     Req, State}.

-spec accept_json_create_service_port(cowboy_req:req(),
                                      #create_service_port_seq{}) -> {{true,
                                                                       binary()},
                                                                      cowboy_req:req(),
                                                                      #create_service_port_seq{}}.

accept_json_create_service_port(Req, State) ->
    #create_service_port_seq{username = Username} = State,
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
