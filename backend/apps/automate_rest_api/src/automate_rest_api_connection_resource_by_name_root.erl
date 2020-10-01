%%% @doc
%%% REST endpoint to manage connection resources.
%%% @end

-module(automate_rest_api_connection_resource_by_name_root).
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

-record(state, { connection_id :: binary()
               , owner :: owner_id() | undefined
               , resource_name :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ConnectionId = cowboy_req:binding(connection_id, Req),
    ResourceName = cowboy_req:binding(resource_name, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ connection_id=ConnectionId
            , owner=undefined
            , resource_name=ResourceName
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{connection_id=ConnectionId}) ->
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
                            case automate_service_port_engine:get_connection_owner(ConnectionId) of
                                {ok, {user, UserId}} ->
                                    { true, Req1, State#state{ owner={user, UserId} } };
                                {ok, _ } ->
                                    { { false, <<"Unauthorized">>}, Req1, State }
                                end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.



%% PATCH handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json}],
     Req, State}.

accept_json(Req, State) ->
    case cowboy_req:method(Req) of
        <<"PATCH">> ->
            patch_json(Req, State)
    end.

patch_json(Req, State=#state{ connection_id=ConnectionId, resource_name=ResourceName }) ->
    {ok, Body, _} = ?UTILS:read_body(Req),
    Data = jiffy:decode(Body, [return_maps]),
    case Data of
        #{ <<"shared">> := Shares } when is_map(Shares) ->
            ok = automate_service_port_engine:set_shared_resource(ConnectionId, ResourceName, Shares)
    end,
    {true, Req, State}.
