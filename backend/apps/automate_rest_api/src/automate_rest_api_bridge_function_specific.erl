%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_bridge_function_specific).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_function_call/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").

-record(state, { user_id :: binary(), bridge_id :: binary(), function_name :: binary() }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    BridgeId = cowboy_req:binding(bridge_id, Req),
    FunctionName = cowboy_req:binding(function, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Req1
    , #state{ user_id=UserId
            , bridge_id=BridgeId
            , function_name=FunctionName
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
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
                    #state{user_id=UserId} = State,
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _TokenUserId} -> %% Non matching user_id
                            { { false, <<"Unauthorized to create a program here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_function_call}],
     Req, State}.

accept_function_call(Req, State) ->
    #state{bridge_id=BridgeId, function_name=FunctionName, user_id=UserId} = State,
    {ok, Body, _} = ?UTILS:read_body(Req),
    #{<<"arguments">> := Arguments } = jiffy:decode(Body, [return_maps]),

    case automate_rest_api_backend:bridge_function_call({user, UserId}, BridgeId, FunctionName, Arguments) of
        {ok, Result } ->
            Output = encode_result(Result),

            Res1 = cowboy_req:set_resp_body(Output, Req),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { true, Res3, State };
        {error, Reason} ->
            Code = case Reason of
                       not_found -> 404;
                       unauthorized -> 403;
                       _ -> 500
                   end,
            Output = jiffy:encode(#{ <<"success">> => false, <<"message">> => Reason }),
            Res = cowboy_req:reply(Code, #{ <<"content-type">> => <<"application/json">> }, Output, Req),
            {stop, Res, State}
    end.

%% Helper functions
encode_result(#{ <<"success">> := true, <<"result">> := Result }) ->
    %% Positive result, just send the necessary data.
    jiffy:encode(#{ <<"result">> => Result
                  , <<"success">> => true
                  });
encode_result(Result) ->
    %% Not a positive result, just encode the returned data to help debugging.
    jiffy:encode(Result).
