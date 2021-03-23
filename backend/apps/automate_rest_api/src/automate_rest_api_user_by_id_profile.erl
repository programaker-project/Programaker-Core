%%% @doc
%%% REST endpoint to manage user profile settings
%%% @end

-module(automate_rest_api_user_by_id_profile).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_accepted/2
        , resource_exists/2
        ]).

-export([ accept_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").

-record(state, { user_id :: binary() }).

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
                    case automate_rest_api_backend:is_valid_token_uid(X, edit_user_profile) of
                        {true, UserId} ->
                            { true, Req1, State };
                        {true, _} -> %% Non matching user id
                            { { false, <<"Unauthorized">>}, Req1, State };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% POST handler
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json}],
     Req, State}.

-spec accept_json(cowboy_req:req(), #state{}) -> {'true',cowboy_req:req(), #state{}}.
accept_json(Req, State=#state{user_id=UserId}) ->
    {ok, Body, Req1} = ?UTILS:read_body(Req),
    #{ <<"groups">> := Groups } = jiffy:decode(Body, [return_maps]),
    case automate_storage:set_owner_public_listings({user, UserId}, Groups) of
        ok ->
            { true, ?UTILS:send_json_output(jiffy:encode(#{ success => true }), Req1), State };
        {error, Reason} ->
            automate_logging:log_api(error, ?MODULE, Reason),
            { false, ?UTILS:send_json_output(jiffy:encode(#{ success => false }), Req1), State }
    end.
