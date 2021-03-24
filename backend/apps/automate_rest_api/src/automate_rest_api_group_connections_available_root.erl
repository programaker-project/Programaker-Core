%%% @doc
%%% REST endpoint to get available connection points
%%% @end

-module(automate_rest_api_group_connections_available_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , resource_exists/2
        ]).

-export([ to_json/2
        ]).


-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { group_id :: binary(), user_id :: binary() | undefined }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    GroupId = cowboy_req:binding(group_id, Req),
    {cowboy_rest, Req
    , #state{ group_id=GroupId
            , user_id=undefined
            }}.

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
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{group_id=GroupId}) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X, {list_group_connections_available, GroupId}) of
                        {true, UserId} ->
                            case automate_storage:is_allowed_to_write_in_group({user, UserId}, GroupId) of
                                true -> { true, Req1, State#state{ user_id=UserId } };
                                false ->
                                    { { false, <<"Operation not allowed">>}, Req1, State }
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{group_id=GroupId}) ->
    case automate_rest_api_backend:list_available_connections({group, GroupId}) of
        { ok, Connections } ->

            Output = jiffy:encode(lists:map(fun to_map/1, Connections)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.

to_map({#service_port_entry{ id=Id
                           , name=Name
                           , owner={OwnerType, OwnerId}
                           }
       , #service_port_configuration{ service_id=ServiceId }
       }) ->
    #{ <<"id">> => Id
     , <<"name">> => Name
     , <<"owner">> => OwnerId
     , <<"owner_full">> => #{ type => OwnerType, id => OwnerId }
     , <<"service_id">> => ServiceId
     }.
