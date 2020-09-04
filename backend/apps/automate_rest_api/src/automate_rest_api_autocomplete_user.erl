%%% @doc
%%% REST endpoint to manager user name autocompletion.
%%% @end

-module(automate_rest_api_autocomplete_user).
-export([init/2]).
-export([ allowed_methods/2
        , options/2
        , is_authorized/2
        , content_types_provided/2
        , malformed_request/2
        ]).

-export([ to_json/2
        ]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(state, { query :: binary() | undefined }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    Qs = cowboy_req:parse_qs(Req),
    Query = proplists:get_value(<<"q">>, Qs),
    {cowboy_rest, Req, #state{ query=Query }}.

malformed_request(Req, State=#state{query=Query}) ->
    case Query of
        undefined ->  % Query is required
            {true, Req, State};
        _ ->
            {false, Req, State}
    end.


%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

%% Authentication
-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X) of
                        {true, _UserId} ->
                            { true, Req1, State };
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
to_json(Req, State=#state{query=Query}) ->
    case automate_storage:search_users(Query) of
        { ok, Users } ->
            Output = jiffy:encode(encode_user_list(Users)),
            Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
            Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

            { Output, Res2, State }
    end.

encode_user_list(Users) ->
    #{ users => lists:map(fun encode_user/1, Users)
     }.

encode_user(#registered_user_entry{ id=Id
                                  , username=Username
                                  }) ->
    #{ id => Id
     , username => Username
     }.
