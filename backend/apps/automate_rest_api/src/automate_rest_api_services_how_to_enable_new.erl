%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_services_how_to_enable_new).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { group_id :: binary() | undefined
               , program_id :: binary() | undefined
               , owner :: owner_id() | undefined
               , service_id :: binary()
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ServiceId = cowboy_req:binding(service_id, Req),
    Qs = cowboy_req:parse_qs(Req),
    GroupId = proplists:get_value(<<"group_id">>, Qs),
    ProgramId = proplists:get_value(<<"program_id">>, Qs),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ group_id=GroupId
            , service_id=ServiceId
            , program_id=ProgramId
            , owner=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId, group_id=GroupId}) ->
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
                            case {ProgramId, GroupId} of
                                {Pid, _} when is_binary(Pid) ->
                                    {ok, #user_program_entry{ owner=Owner }} = automate_storage:get_program_from_id(ProgramId),
                                    case automate_storage:can_user_edit_as({user, UserId}, Owner) of
                                        true -> { true, Req1, State#state{ owner=Owner } };
                                        false ->
                                            { { false, <<"Operation not allowed">>}, Req1, State }
                                    end;
                                {undefined, G} when is_binary(G) ->
                                    case automate_storage:is_allowed_to_write_in_group({user, UserId}, GroupId) of
                                        true ->
                                            { true, Req1, State#state{owner={group, GroupId}} };
                                        false ->
                                            { { false, <<"Unauthorized to create a service here">>}, Req1, State }
                                    end;
                        {undefined, undefined} ->
                            { true, Req1, State#state{owner={user, UserId}} }
                        end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% Get handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State=#state{owner=Owner, service_id=ServiceId}) ->
    case get_how_to(Owner, ServiceId) of
        { ok, HowTo } ->
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { jiffy:encode(extend_how_to(HowTo, ServiceId)), Res2, State };
        {error, Reason} ->
            automate_logging:log_api(error, ?MODULE, binary:list_to_bin(lists:flatten(io_lib:format("~p", [Reason])))),
            Code = case Reason of
                       not_found -> 404;
                       no_connection -> 409;
                       _ -> 500
                   end,
            Output = jiffy:encode(#{ <<"success">> => false
                                   , <<"message">> => case Reason of
                                                          X when is_atom(X) -> X;
                                                          _ -> error
                                                      end
                                   }),
            Res = cowboy_req:reply(Code, #{ <<"content-type">> => <<"application/json">> }, Output, Req),
            { stop, Res, State }
    end.

get_how_to(Owner, ServiceId) ->
    case automate_service_registry:get_service_by_id(ServiceId) of
        E = {error, _} ->
            E;
        {ok, #{ module := Module }} ->
            automate_service_registry_query:get_how_to_enable(Module, Owner)
    end.

extend_how_to(HowTo=#{ <<"type">> := <<"form">>
                     , <<"connection_id">> := ConnectionId
                     }, ServiceId) ->
    Restructured = HowTo#{ <<"metadata">> => #{ <<"service_id">> => ServiceId
                                              , <<"connection_id">> => ConnectionId
                                              } },
    maps:remove(<<"connection_id">>, Restructured);

extend_how_to(HowTo=#{ <<"type">> := <<"message">>
                     , <<"connection_id">> := ConnectionId }, ServiceId) ->
    Restructured = HowTo#{ <<"metadata">> => #{ <<"service_id">> => ServiceId
                                              , <<"connection_id">> => ConnectionId
                                              } },
    maps:remove(<<"connection_id">>, Restructured);

extend_how_to(HowTo=#{ <<"type">> := <<"form">> }, ServiceId) ->
    HowTo#{ <<"metadata">> => #{ <<"service_id">> => ServiceId } };

extend_how_to(HowTo=#{ <<"type">> := <<"message">> }, ServiceId) ->
    HowTo#{ <<"metadata">> => #{ <<"service_id">> => ServiceId } };

extend_how_to(HowTo=#{ <<"type">> := <<"direct">> }, ServiceId) ->
    HowTo#{ <<"metadata">> => #{ <<"service_id">> => ServiceId } };

extend_how_to(HowTo, _ServiceId) ->
    HowTo.
