%%% @doc
%%% REST endpoint to manage group programs.
%%% @end

-module(automate_rest_api_group_shared_resources).
-export([init/2]).
-export([ allowed_methods/2
        , is_authorized/2
        , content_types_provided/2
        , options/2
        ]).

-export([ to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(FORMATTING, automate_rest_api_utils_formatting).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { group_id :: binary()}).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    GroupId = cowboy_req:binding(group_id, Req),
    {cowboy_rest, Req, #state{ group_id=GroupId }}.

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
is_authorized(Req, State=#state{group_id=GroupId}) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        <<"GET">> ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    case automate_rest_api_backend:is_valid_token_uid(X, { list_group_shares, GroupId }) of
                        {true, UserId} ->
                            case automate_storage:is_allowed_to_read_in_group({user, UserId}, GroupId) of
                                true -> { true, Req1, State };
                                false ->
                                    { { false, <<"Operation not allowed">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.


-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{group_id=GroupId}) ->
    case automate_service_port_engine:get_resources_shared_with({group, GroupId}) of
        { ok, Shares } ->
            Data = lists:map(fun(#bridge_resource_share_entry{ connection_id=ConnectionId
                                                             , resource=Resource
                                                             , value=Value
                                                             }) ->
                                     {ok, {ConnOwnerType, ConnOwnerId}} = automate_service_port_engine:get_connection_owner(ConnectionId),
                                     {ok, BridgeId} = automate_service_port_engine:get_connection_bridge(ConnectionId),
                                     {ok, #service_port_metadata{icon=Icon, name=Name}} = automate_service_port_engine:get_bridge_info(BridgeId),
                                     #{ bridge_id => BridgeId
                                      , icon => ?FORMATTING:serialize_icon(Icon)
                                      , name => Name
                                      , resource => Resource
                                      , value_id => Value
                                      , shared_by => #{ type => ConnOwnerType, id => ConnOwnerId}
                                      }
                             end, Shares),
            Output = jiffy:encode(
                       #{ success => true
                        , resources => Data
                        }),
            Res = ?UTILS:send_json_format(Req),

            { Output, Res, State }
    end.
