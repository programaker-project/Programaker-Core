%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_service_ports_connections).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).

-export([ accept_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { bridge_id :: binary()
               , group_id :: binary() | undefined
               , owner :: owner_id() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Qs = cowboy_req:parse_qs(Req),
    GroupId = proplists:get_value(<<"group_id">>, Qs),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ bridge_id=BridgeId
            , group_id=GroupId
            , owner=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{ group_id=GroupId }) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            case GroupId of
                                undefined ->
                                    { true, Req1, State#state{ owner={user, UserId} } };
                                GId when is_binary(GId) ->
                                    case automate_storage:is_allowed_to_write_in_group({user, UserId}, GroupId) of
                                        true ->
                                            { true, Req1, State#state{ owner={group, GroupId} } };
                                        false ->
                                            { { false, <<"Unauthorized">>}, Req1, State }
                                        end
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% Route by Method
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json}],
     Req, State}.

accept_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> ->
            accept_json_patch(Req, State)
    end.

%% PATCH handler
accept_json_patch(Req, State=#state{bridge_id=BridgeId, owner=Owner}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Parsed = jiffy:decode(Body, [return_maps]),
    case Parsed of
        #{ <<"save_signals">> := SaveSignals } ->
            case automate_service_port_engine:set_save_signals_from_bridge(BridgeId, Owner, SaveSignals) of
                ok ->
                    Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true }), Req),
                    { true, Req2, State };
                { error, Reason } ->
                    Req2 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }), Req1),
                    { false, Req2, State }
            end
    end.
