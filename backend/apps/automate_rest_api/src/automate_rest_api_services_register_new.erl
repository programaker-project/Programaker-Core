%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_services_register_new).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        ]).

-export([ accept_json_register_service/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").

-record(state, { user_id :: binary() | undefined
               , group_id :: binary()
               , service_id :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ServiceId = cowboy_req:binding(service_id, Req),
    Qs = cowboy_req:parse_qs(Req),
    GroupId = proplists:get_value(<<"group_id">>, Qs),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ user_id=undefined
            , group_id=GroupId
            , service_id=ServiceId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            case GroupId of
                                G when is_binary(G) ->
                                    case automate_storage:is_allowed_to_write_in_group({user, UserId}, GroupId) of
                                        true ->
                                            { true, Req1, State#state{user_id=UserId} };
                                        false ->
                                            { { false, <<"Unauthorized to create a service here">>}, Req1, State }
                                    end
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.


%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []},
       accept_json_register_service}],
     Req, State}.

-spec accept_json_register_service(cowboy_req:req(),
                                   #state{}) -> {true, cowboy_req:req(), #state{}}.
accept_json_register_service(Req, State=#state{group_id=GroupId, service_id=ServiceId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    FullRegistrationData = jiffy:decode(Body, [return_maps]),
    { RegistrationData, ConnectionId } = case FullRegistrationData of
                                             #{ <<"metadata">> := #{<<"connection_id">> := ConnId} } ->
                                                 {maps:remove(<<"metadata">>, FullRegistrationData), ConnId};
                                             #{ <<"metadata">> := #{} } ->
                                                 {maps:remove(<<"metadata">>, FullRegistrationData), undefined};
                                             _ ->
                                                 {FullRegistrationData, undefined}
                                         end,
    case send_registration_data({group, GroupId}, ServiceId, RegistrationData, ConnectionId) of
        {ok, Data} ->
            Output = jiffy:encode(Data),
            Res2 = ?UTILS:send_json_output(Output, Req1),
            {true, Res2, State}
    end.

send_registration_data(Owner, ServiceId, RegistrationData, ConnectionId) ->
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId),
    {ok, _Result} = automate_service_registry_query:send_registration_data(Module, Owner, RegistrationData,
                                                                           #{<<"connection_id">> => ConnectionId}).
