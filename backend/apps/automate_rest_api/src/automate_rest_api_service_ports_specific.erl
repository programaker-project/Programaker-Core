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

-record(state, { bridge_id :: binary()
               , permissions :: owner_id() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ bridge_id=BridgeId
            , permissions=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{bridge_id=BridgeId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        Method ->
            Check = case Method of
                        <<"DELETE">> -> fun automate_storage:can_user_admin_as/2
            end,
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X, { delete_bridge, BridgeId }) of
                        {true, UserId} ->
                            {ok, Owner} = automate_service_port_engine:get_bridge_owner(BridgeId),
                            case Check({user, UserId}, Owner) of
                                true -> { true, Req1, State#state{ permissions=Owner } };
                                _ ->
                                    automate_logging:log_api(warning, ?MODULE, io_lib:format("Resource owner: ~p | Token UID: ~p~n", [Owner, UserId])),
                                    { { false, <<"Unauthorized operation">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.


%% DELETE handler
delete_resource(Req, State=#state{bridge_id=BridgeId, permissions=Owner}) ->
    case automate_service_port_engine:delete_bridge(Owner, BridgeId) of
        ok ->
            Req1 = send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        { error, Reason } ->
            Req1 = send_json_output(jiffy:encode(#{ <<"success">> => false, <<"debug">> => list_to_binary(io_lib:format("~p", [Reason])) }), Req),
            { false, Req1, State }
    end.


send_json_output(Output, Req) ->
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2).
