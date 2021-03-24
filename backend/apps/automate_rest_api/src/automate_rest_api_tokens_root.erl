%%% @doc
%%% REST endpoint to manage user tokens.
%%% @end

-module(automate_rest_api_tokens_root).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_accepted/2
        , options/2
        , is_authorized/2
        ]).
-export([accept_json/2]).

-define(UTILS, automate_rest_api_utils).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { user_id :: binary() | undefined
               }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req
    , #state{ user_id=undefined
            }}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X, create_api_tokens) of
                        {true, UserId} ->
                            { true, Req1, #state{user_id=UserId} };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json}],
     Req, State}.

%%%% POST
-spec accept_json(cowboy_req:req(),#state{}) -> {'true',cowboy_req:req(),_}.
accept_json(Req, Session=#state{ user_id=UserId }) ->
    {ok, Body, Req2} = ?UTILS:read_body(Req),
    #{ <<"scopes">> := ScopesStr } = jiffy:decode(Body, [return_maps]),
    Scopes = parse_scopes(ScopesStr),
    case automate_storage:generate_token_for_user(UserId, Scopes, never) of
        { ok, Token } ->
            Output = jiffy:encode(#{ success => true
                                   , value => #{ token => Token
                                               }
                                   }),
            Res1 = cowboy_req:set_resp_body(Output, Req2),
            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
            { true, Res3, Session };

        {error, Reason} ->
            Res1 = cowboy_req:set_resp_body(jiffy:encode(#{ success => false
                                                          , error => reason_to_json(Reason)
                                                          }), Req2),
            automate_logging:log_api(error, ?MODULE, Reason),
            { false, Res1, Session}
    end.

-spec parse_scopes(binary() | [binary()]) -> session_scope().
parse_scopes(<<"all">>) ->
    all;
parse_scopes(Scopes) when is_list(Scopes) ->
    lists:map(fun parse_single_scope/1, Scopes).

parse_single_scope(<<"list_bridges">>) ->
    list_bridges;
parse_single_scope(<<"list_custom_blocks">>) ->
    list_custom_blocks;
parse_single_scope(<<"list_connections_established">>) ->
    list_connections_established;
parse_single_scope(<<"call_any_bridge">>) ->
    call_any_bridge.

reason_to_json(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).
