%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_program_services_root).
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
-include("../../automate_storage/src/records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_service_registry/src/records.hrl").

-record(state, { owner :: owner_id() | undefined
               , program_id :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    {cowboy_rest, Req
    , #state{ program_id=ProgramId
            , owner=undefined
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

is_authorized(Req, State=#state{program_id=ProgramId}) ->
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
                            {ok, #user_program_entry{ owner=Owner }} = automate_storage:get_program_from_id(ProgramId),
                            case automate_storage:can_user_view_as({user, UserId}, Owner) of
                                true -> { true, Req1, State#state{ owner=Owner } };
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
to_json(Req, State=#state{owner=Owner}) ->
    {ok, Services} =  automate_service_registry:get_all_services_for_user(Owner),
    {ok, SharedConnections} = automate_service_port_engine:get_resources_shared_with(Owner),

    SharedBridges = lists:map(fun(#bridge_resource_share_entry{ connection_id=ConnectionId }) ->
                                      {ok, BridgeId} = automate_service_port_engine:get_connection_bridge(ConnectionId),
                                      BridgeId
              end, SharedConnections),
    SharedServices = lists:map(fun(BridgeId) ->
                                       {ok, #service_port_configuration{service_id=ServiceId}} = automate_service_port_engine:get_bridge_configuration(BridgeId),
                                       {ok, Service} = automate_service_registry:get_service_by_id(ServiceId),
                                       {ServiceId, Service}
                               end, sets:to_list(sets:from_list(SharedBridges))),

    AllServices = merge_service_map(SharedServices,Services),

    ServiceData = automate_rest_api_backend:get_services_metadata(AllServices, Owner),
    Output = jiffy:encode(encode_service_list(ServiceData)),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

    { Output, Res2, State }.

merge_service_map([], Acc) ->
    Acc;
merge_service_map([{Id, Val} | T], Acc) ->
    merge_service_map(T, Acc#{ Id => Val }).


encode_service_list(Services) ->
    encode_service_list(Services, []).


encode_service_list([], Acc) ->
    lists:reverse(Acc);

encode_service_list([H | T], Acc) ->
    #service_metadata{ id=Id
                     , name=Name
                     , link=Link
                     , enabled=Enabled
                     } = H,
    AsDictionary = #{ <<"id">> => Id
                    , <<"name">> => Name
                    , <<"link">> =>  Link
                    , <<"enabled">> => Enabled
                    },
    encode_service_list(T, [AsDictionary | Acc]).
