%%% @doc
%%% REST endpoint to inspect user preferences.
%%% @end

-module(automate_rest_api_sessions_check).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_provided/2
        , options/2
        , is_authorized/2
        ]).
-export([to_json/2]).

-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-record(check_seq, { user_id :: binary() | undefined }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req
    , #check_seq{ user_id=undefined }}.

content_types_provided(Req, State) ->
    {[ {<<"application/json">>, to_json}
     ], Req, State}.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

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
                    case automate_rest_api_backend:is_valid_token_uid(X, check) of
                        {true, UserId} ->
                            { true, Req1, #check_seq{user_id=UserId} };
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% GET handler
-spec to_json(cowboy_req:req(), #check_seq{}) -> {binary(),cowboy_req:req(),_}.
to_json(Req, State=#check_seq{user_id=UserId}) ->
    {ok, User} = automate_rest_api_backend:get_user(UserId),

    Output = encode_user(User),
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Res2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1),

    { Output, Res2, State }.


encode_user(#registered_user_entry{ id=UserId
                                  , canonical_username=Username
                                    %% , password
                                    %% , email
                                    %% , status
                                    %% , registration_time

                                  , is_admin=IsAdmin
                                  , is_advanced=IsAdvanced
                                  , is_in_preview=IsInPreview
                                  }) ->
    jiffy:encode(#{ success => true
                  , username => Username
                  , user_id => UserId
                  , tags => #{ is_admin => IsAdmin
                             , is_advanced => IsAdvanced
                             , is_in_preview => IsInPreview
                             }
                  }).
