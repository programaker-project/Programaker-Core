%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_bridge_tokens_by_name_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , delete_resource/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { bridge_id :: binary()
               , owner :: owner_id() | undefined
               , group_id :: binary() | undefined
               , token_name :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    BridgeId = cowboy_req:binding(bridge_id, Req),
    TokenName = cowboy_req:binding(token_name, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    Qs = cowboy_req:parse_qs(Req),
    GroupId = proplists:get_value(<<"group_id">>, Qs),
    {cowboy_rest, Req1
    , #state{ bridge_id=BridgeId
            , token_name=TokenName
            , owner=undefined
            , group_id=GroupId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{ bridge_id=BridgeId }) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X, { delete_bridge_tokens, BridgeId }) of
                        {true, UserId} ->
                            {ok, Owner} = automate_service_port_engine:get_bridge_owner(BridgeId),
                            case automate_storage:can_user_admin_as({user, UserId}, Owner) of
                                true -> { true, Req1, State#state{ owner=Owner } };
                                false ->
                                    { { false, <<"Operation not allowed">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% DELETE handler
delete_resource(Req, State=#state{ bridge_id=BridgeId, token_name=TokenName }) ->
    case automate_service_port_engine:delete_bridge_token_by_name(BridgeId, TokenName) of
        ok ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => true}), Req),
            { true, Req1, State };
        {error, not_found} ->
            Req1 = ?UTILS:send_json_output(jiffy:encode(#{ <<"success">> => false, debug => not_found}), Req),
            { false, Req1, State }
    end.
