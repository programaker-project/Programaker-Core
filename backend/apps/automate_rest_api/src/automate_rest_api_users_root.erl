%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_users_root).
-export([init/2]).
-export([ allowed_methods/2
        , is_authorized/2
        , content_types_provided/2
        , options/2
        ]).

-export([ to_json/2
        ]).
-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
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
                        {true, UserId} ->
                            case automate_storage:get_user(UserId) of
                                {ok, #registered_user_entry{ is_admin=true }} ->
                                    { true, Req1, State };
                                {ok, _} ->
                                    { { false, <<"User not authorized (not admin)">>}, Req1, State };
                                {error, Reason} ->
                                    automage_logging:log_api(error, ?MODULE, Reason)
                            end;
                        false ->
                            { { false, <<"Authorization not correct">>}, Req1, State }
                    end
            end
    end.

%% CORS
options(Req, State) ->
    Req1 = automate_rest_api_cors:set_headers(Req),
    {ok, Req1, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    io:fwrite("[~p] Asking for methods~n", [?MODULE]),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}],
     Req, State}.

%%%% GET
-spec to_json(cowboy_req:req(),#rest_session{}) -> {binary(),cowboy_req:req(),_}.
to_json(Req, Session) ->
    {ok, Result} = automate_storage:admin_list_users(),
    Output = jiffy:encode(lists:map(fun(U) -> full_serialize_user(U) end, Result)),
    Res = ?UTILS:send_json_format(Req),
    { Output, Res, Session }.


%% Users
full_serialize_user({#registered_user_entry{ id=UserId
                                           , username=_Username
                                           , canonical_username=CanonicalUsername
                                           , password=_
                                           , email=Email
                                           , status=Status
                                           , registration_time=RegistrationTime

                                           , is_admin=IsAdmin
                                           , is_advanced=IsAdvanced
                                           , is_in_preview=IsInPreview
                                           }
                    , LastActiveTime
                    }) ->
    #{ success => true
     , username => CanonicalUsername
     , user_id => UserId
     , email => Email
     , status => Status
     , registration_time => number_or_undefined(RegistrationTime)
     , last_active_time => number_or_undefined(LastActiveTime)
     , tags => #{ is_admin => IsAdmin
                , is_advanced => IsAdvanced
                , is_in_preview => IsInPreview
                }
     }.

number_or_undefined(undefined) ->
    null;
number_or_undefined(none) ->
    null;
number_or_undefined(Num) ->
    Num.
