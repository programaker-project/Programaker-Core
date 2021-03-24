%%% @doc
%%% REST endpoint to manage bridge.
%%% @end

-module(automate_rest_api_bridge_tokens_root).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ to_json/2
        , accept_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-define(URLS, automate_rest_api_utils_urls).
-include("./records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { bridge_id :: binary()
               , owner :: owner_id() | undefined
               , group_id :: binary() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    BridgeId = cowboy_req:binding(bridge_id, Req),
    Req1 = automate_rest_api_cors:set_headers(Req),
    Qs = cowboy_req:parse_qs(Req),
    GroupId = proplists:get_value(<<"group_id">>, Qs),
    {cowboy_rest, Req1
    , #state{ bridge_id=BridgeId
            , owner=undefined
            , group_id=GroupId
            }}.

%% CORS
options(Req, State) ->
    {ok, Req, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State=#state{ bridge_id=BridgeId }) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    case cowboy_req:method(Req1) of
        %% Don't do authentication if it's just asking for options
        <<"OPTIONS">> ->
            { true, Req1, State };
        Method ->
            case cowboy_req:header(<<"authorization">>, Req, undefined) of
                undefined ->
                    { {false, <<"Authorization header not found">>} , Req1, State };
                X ->
                    Scope = case Method of
                                <<"GET">> -> {list_bridge_tokens, BridgeId};
                                <<"POST">> -> {create_bridge_tokens, BridgeId}
                            end,
                    case automate_rest_api_backend:is_valid_token_uid(X, Scope) of
                        {true, UserId} ->
                            {ok, Owner} = automate_service_port_engine:get_bridge_owner(BridgeId),
                            case automate_storage:can_user_admin_as({user, UserId}, Owner) of
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

to_json(Req, State=#state{ bridge_id=BridgeId }) ->
    case automate_service_port_engine:list_bridge_tokens(BridgeId) of
        {ok, Tokens} ->
            Data = lists:map(fun(#bridge_token_entry{token_name=Name}) ->
                                        #{ name => Name }
                                end, Tokens),
            Output = jiffy:encode(
                       #{ success => true
                        , tokens => Data
                        }),

            Res = ?UTILS:send_json_format(Req),

            { Output, Res, State }
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []},
       accept_json}],
     Req, State}.

-spec accept_json(cowboy_req:req(), #state{}) -> {{true, iolist()}, cowboy_req:req(), #state{}}.
accept_json(Req, State=#state{owner=Owner, bridge_id=BridgeId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    #{ <<"name">> := TokenName
     } = Data = jiffy:decode(Body, [return_maps]),
    ExpiresOn = case Data of
                    #{ <<"expires_in">> := _ExpiresOn } ->
                        undefined;
                    _ ->
                        undefined
                end,

    case automate_service_port_engine:create_bridge_token(BridgeId, Owner, TokenName, ExpiresOn) of
        {ok, TokenKey} ->
            Output = jiffy:encode(#{ name => TokenName
                                   , key => TokenKey
                                   }),
            Res2 = ?UTILS:send_json_output(Output, Req1),

            { {true, ?URLS:bridge_token_by_name_url(BridgeId, TokenName)}, Res2, State};
        {error, name_taken} ->
            Output = jiffy:encode(
                       #{ success => false
                        , error => name_taken
                        }),

            ConflictStatusCode = 409,

            Res = cowboy_req:reply(ConflictStatusCode, #{ <<"content-type">> => <<"application/json">> }, Output, Req),
            { stop, Res, State }

    end.

%% Declare resources being created as not existing yet in this endpoint.
resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {false, Req, State};
        _ ->
            { true, Req, State }
    end.
