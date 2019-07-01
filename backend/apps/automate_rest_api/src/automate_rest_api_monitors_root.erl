%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_monitors_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_json_create_monitor/2
        , to_json/2
        ]).

-include("./records.hrl").

-record(state, { username }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #state{ username=UserId }}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            { false, Req, State };
        _ ->
            { true, Req, State}
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("Asking for methods~n", []),
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

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
                            { { false, <<"Unauthorized to create a monitor here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_create_monitor}],
     Req, State}.

-spec accept_json_create_monitor(cowboy_req:req(), #state{})
                                -> {'true',cowboy_req:req(), #state{}}.
accept_json_create_monitor(Req, State) ->
    #state{username=Username} = State,

    {ok, Body, Req1} = read_body(Req),
    Parsed = [jiffy:decode(Body, [return_maps])],
    Monitor = decode_monitor(Parsed),

    case automate_rest_api_backend:create_monitor(Username, Monitor) of
        { ok, {MonitorId, MonitorName} } ->

            Output = jiffy:encode(#{ <<"id">> => MonitorId
                                   , <<"name">> => MonitorName
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req1),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { true, Res3, State }
    end.

decode_monitor([#{ <<"type">> := Type
                 , <<"value">> := Value
                 , <<"name">> := Name
                 }]) ->
    #monitor_descriptor{ type=Type
                       , value=Value
                       , name=Name
                       }.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.



%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{username=Username} = State,
    case automate_rest_api_backend:lists_monitors_from_username(Username) of
        { ok, Monitors } ->

            Output = jiffy:encode(encode_monitors_list(Monitors)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.


encode_monitors_list(Monitors) ->
    encode_monitors_list(Monitors, []).

encode_monitors_list([], Acc) ->
    lists:reverse(Acc);

encode_monitors_list([H | T], Acc) ->
    #monitor_metadata{ id=Id
                     , name=Name
                     , link=Link
                     } = H,
    AsDictionary = #{ <<"id">> => Id
                    , <<"name">> => Name
                    , <<"link">> =>  Link
                    },
    encode_monitors_list(T, [AsDictionary | Acc]).
