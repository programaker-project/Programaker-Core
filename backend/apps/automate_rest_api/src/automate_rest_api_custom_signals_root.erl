%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_custom_signals_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_json_create_signal/2
        , to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { user_id }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    UserId = cowboy_req:binding(user_id, Req),
    {cowboy_rest, Req
    , #state{ user_id=UserId }}.

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
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

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
                        {true, _} -> %% Non matching user id
                            { { false, <<"Unauthorized to create a template here">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_create_signal}],
     Req, State}.

-spec accept_json_create_signal(cowboy_req:req(), #state{})
                                 -> {'true',cowboy_req:req(), #state{}}.
accept_json_create_signal(Req, State) ->
    #state{user_id=UserId} = State,

    {ok, Body, Req1} = ?UTILS:read_body(Req),
    Signal = jiffy:decode(Body, [return_maps]),
    #{ <<"name">> := SignalName } = Signal,

    case automate_rest_api_backend:create_custom_signal(UserId, SignalName) of
        { ok, SignalId } ->

            Output = jiffy:encode(#{ <<"id">> => SignalId
                                   }),

            Res1 = cowboy_req:set_resp_body(Output, Req1),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),

            { true, Res3, State }
    end.

%% GET handler
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

-spec to_json(cowboy_req:req(), #state{})
             -> {binary(),cowboy_req:req(), #state{}}.
to_json(Req, State) ->
    #state{user_id=UserId} = State,
    case automate_rest_api_backend:list_custom_signals_from_user_id(UserId) of
        { ok, Signals } ->

            Output = jiffy:encode(lists:map(fun signal_to_map/1, Signals)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.

signal_to_map(#custom_signal_entry{ id=Id
                                  , name=Name
                                  , owner=Owner
                                  }) ->
    #{ id => Id
     , name => Name
     , owner => Owner
     }.
