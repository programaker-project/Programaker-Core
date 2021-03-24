%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_program_bridge_callback).
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

-record(state, { owner :: owner_id() | undefined
               , program_id :: binary()
               , bridge_id :: binary()
               , callback :: binary()
               , sequence_id :: binary() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    ProgramId = cowboy_req:binding(program_id, Req),
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Callback = cowboy_req:binding(callback, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),

    Qs = cowboy_req:parse_qs(Req1),
    SequenceId = proplists:get_value(<<"sequence_id">>, Qs),

    {cowboy_rest, Req1
    , #state{ owner=undefined
            , program_id=ProgramId
            , bridge_id=BridgeId
            , callback=Callback
            , sequence_id=SequenceId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{program_id=ProgramId, bridge_id=BridgeId}) ->
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
                    case automate_rest_api_backend:is_valid_token_uid(X, { call_program_bridge_callback, BridgeId, ProgramId }) of
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

to_json(Req, State=#state{bridge_id=BridgeId, callback=Callback, owner=Owner, sequence_id=SequenceId}) ->
    case automate_service_port_engine:callback_bridge(Owner, BridgeId, Callback, SequenceId) of
        {ok, Result} ->
            Output = jiffy:encode(#{ success => true, result => Result }),
            Res = ?UTILS:send_json_format(Req),

            { Output, Res, State };
        {error, Reason} ->
            Code = case Reason of
                       not_found -> 404;
                       unauthorized -> 403;
                       no_connection -> 409; %% Conflict
                       _ -> 500
                   end,
            Output = jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }),
            Res = cowboy_req:reply(Code, #{ <<"content-type">> => <<"application/json">> }, Output, Req),
            { stop, Res, State }
    end.
