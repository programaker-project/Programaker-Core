%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_service_ports_specific).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , delete_resource/2
        ]).

-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { user_id, bridge_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ user_id=UserId
            , bridge_id=BridgeId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

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
                    #state{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, TokenUserId} -> %% Non matching user_id
                            automate_logging:log_api(warning, ?MODULE, io_lib:format("Url UID: ~p | Token UID: ~p~n", [UserId, TokenUserId])),
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.


%% DELETE handler
delete_resource(Req, State) ->
    #state{bridge_id=BridgeId, user_id=UserId} = State,
    case automate_rest_api_backend:delete_bridge(UserId, BridgeId) of
        ok ->
            Req1 = send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req),
            { false, Req1, State }
    end.


send_json_output(Output, Req) ->
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2).
