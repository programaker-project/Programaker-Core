%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_bridge_resources_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { bridge_id :: binary()
               , owner :: owner_id() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ bridge_id=BridgeId
            , owner=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State#state{ owner={user, UserId} } };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

to_json(Req, State=#state{ bridge_id=BridgeId, owner=Owner }) ->
    {ok, #service_port_configuration{resources=Resources}} = automate_service_port_engine:get_bridge_configuration(BridgeId),
    case automate_service_port_engine:list_established_connections(Owner, BridgeId) of
        {ok, Results} ->

            ResourceList = merge_to_map(lists:flatmap(fun(#user_to_bridge_connection_entry{id=ConnectionId}) ->
                                                                lists:map(fun(ResourceName) ->
                                                                                  {ok, #{ <<"result">> := Values }} = automate_service_port_engine:callback_bridge_through_connection(ConnectionId, BridgeId, ResourceName),
                                                                                  {ResourceName, maps:map(fun(_K, V) -> V#{ connection_id => ConnectionId } end, Values)}
                                                                          end, Resources)
                                                        end, Results)),

            Res = ?UTILS:send_json_format(Req),
            { jiffy:encode(ResourceList), Res, State }
    end.

merge_to_map(List) ->
    merge_to_map(List, #{}).

merge_to_map([], Acc) ->
    Acc;
merge_to_map([{K, V} | T], Acc) ->
    case Acc of
        #{ K := Prev } ->
            merge_to_map(T, Acc#{ K => Prev#{ K =>V } });
        _ ->
            merge_to_map(T, Acc#{ K => V } )
    end.
