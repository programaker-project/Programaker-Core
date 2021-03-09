%%% @doc
%%% REST endpoint to pull shared resources from a program.
%%% @end

-module(automate_rest_api_program_shared_resources).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        ]).

-export([ to_json/2
        ]).

-define(FORMATTING, automate_rest_api_utils_formatting).
-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { program_id :: binary()
               , owner_id :: owner_id()|undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ program_id=ProgramId
            , owner_id=undefined
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

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
                            case automate_storage:is_user_allowed({user, UserId}, ProgramId, read_program) of
                                {ok, true} ->
                                    {ok, Owner} = automate_storage:get_program_owner(ProgramId),
                                    { true, Req1, State#state{owner_id=Owner} };
                                {ok, false} ->
                                    { { false, <<"Unauthorized">>}, Req1, State }
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
to_json(Req, State=#state{owner_id=Owner}) ->
    case automate_service_port_engine:get_resources_shared_with(Owner) of
        { ok, Shares } ->
            Data = lists:filtermap(fun(#bridge_resource_share_entry{ connection_id=ConnectionId
                                                             , resource=Resource
                                                             , value=Value
                                                             }) ->
                                           case automate_service_port_engine:get_connection_owner(ConnectionId) of
                                               {ok, {ConnOwnerType, ConnOwnerId}} ->
                                                   {ok, BridgeId} = automate_service_port_engine:get_connection_bridge(ConnectionId),
                                                   {ok, #service_port_metadata{icon=Icon, name=Name}} = automate_service_port_engine:get_bridge_info(BridgeId),
                                                   { true, #{ bridge_id => BridgeId
                                                    , icon => ?FORMATTING:serialize_icon(Icon)
                                                    , name => Name
                                                    , resource => Resource
                                                    , value_id => Value
                                                    , shared_by => #{ type => ConnOwnerType, id => ConnOwnerId}
                                                            }};
                                               {error, not_found} ->
                                                   automate_logging:log_api(error, ?MODULE, binary:list_to_bin(io_lib:format("Bridge not found for connection: ~p", [ConnectionId]))),
                                                   false
                                           end
                             end, Shares),
            Output = jiffy:encode(
                       #{ success => true
                        , resources => Data
                        }),
            Res = ?UTILS:send_json_format(Req),

            { Output, Res, State }
    end.
