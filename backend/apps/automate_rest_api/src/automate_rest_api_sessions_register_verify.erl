%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_sessions_register_verify).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_accepted/2
        , options/2
        ]).

-export([accept_json_modify_collection/2]).

-define(UTILS, automate_rest_api_utils).
-define(FORMAT, automate_rest_api_utils_formatting).
-include("../../automate_storage/src/records.hrl").
-include("./records.hrl").

-record(login_seq, { rest_session,
                     login_data
                   }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req
    , #login_seq{ rest_session=undefined
                , login_data=undefined}}.

%% CORS
options(Req, State) ->
    Res = automate_rest_api_cors:set_headers(Req),
    {ok, Res, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    Res = automate_rest_api_cors:set_headers(Req),
    io:fwrite("[Validate Register] Asking for methods~n", []),
    {[<<"POST">>, <<"OPTIONS">>], Res, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_modify_collection}],
     Req, State}.

%%%% POST
-spec accept_json_modify_collection(cowboy_req:req(),#login_seq{})
                                   -> {'true',cowboy_req:req(),_}.
accept_json_modify_collection(Req, Session) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, Req2} = ?UTILS:read_body(Req),
            Parsed = jiffy:decode(Body, [return_maps]),
            case Parsed of
                #{ <<"verification_code">> := VerificationCode } ->
                    case automate_rest_api_backend:verify_registration_with_code(VerificationCode) of
                        { ok, UserId } ->
                            case automate_rest_api_backend:get_user(UserId) of
                                {ok, #registered_user_entry{ username=Username
                                                           , status=ready
                                                           }} ->
                                    { ok, Token } = automate_rest_api_backend:generate_token_for_user(UserId),
                                    Output = jiffy:encode(#{ <<"success">> => true
                                                           , <<"session">> => #{ <<"token">> => Token
                                                                               , <<"user_id">> => UserId
                                                                               , <<"username">> => Username
                                                                               }
                                                           }),
                                    Res1 = cowboy_req:set_resp_body(Output, Req2),
                                    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
                                    Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
                                    { true, Res3, Session };

                                _ ->
                                    Res1 = cowboy_req:set_resp_body(jiffy:encode(#{ success => false
                                                                                  , error => #{ type => user_not_ready }
                                                                                  }), Req2),
                                    io:format("Error autologin on verify: user not found or not ready~n"),
                                    { false, Res1, Session}
                            end;

                        {error, Reason} ->
                            Res1 = cowboy_req:set_resp_body(jiffy:encode(#{ success => false
                                                                          , error => ?FORMAT:reason_to_json(Reason)
                                                                          }), Req2),
                            io:format("Error logging in: ~p~n", [Reason]),
                            { false, Res1, Session}
                    end;
                _  ->
                    { false, Req2, Session }
            end;
        false ->
            {false, Req, Session }
    end.
