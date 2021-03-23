%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_bridge_callback).
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

-record(state, { user_id :: binary()
               , bridge_id :: binary()
               , callback :: binary()
               , sequence_id :: binary() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Callback = cowboy_req:binding(callback, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),

    Qs = cowboy_req:parse_qs(Req1),
    SequenceId = proplists:get_value(<<"sequence_id">>, Qs),

    {cowboy_rest, Req1
    , #state{ user_id=UserId
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
                    #state{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X, { get_bridge_callback, BridgeId }) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _TokenUserId} -> %% Non matching user_id
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

to_json(Req, State=#state{bridge_id=BridgeId, callback=Callback, user_id=UserId, sequence_id=SequenceId}) ->
    case automate_service_port_engine:callback_bridge({user, UserId}, BridgeId, Callback, SequenceId) of
        {ok, Result} ->
            Output = jiffy:encode(Result),
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
