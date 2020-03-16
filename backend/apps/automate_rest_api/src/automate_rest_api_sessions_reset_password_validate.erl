%%% @doc
%%% REST endpoint that allows to check if a token is valid to perform password updates.
%%% @end

-module(automate_rest_api_sessions_reset_password_validate).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_accepted/2
        , options/2
        ]).

-export([accept_json_modify_collection/2]).

-define(UTILS, automate_rest_api_utils).
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
    io:fwrite("[Password reset/Validate] Asking for methods~n", []),
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
                #{ <<"verification_code">> := VerificationCode} ->
                    case automate_rest_api_backend:check_password_reset_verification_code(VerificationCode) of
                        ok ->
                            Output = jiffy:encode(#{ success => true}),
                            Res1 = cowboy_req:set_resp_body(Output, Req2),
                            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
                            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
                            { true, Res3, Session };

                        {error, Reason} ->
                            Res1 = cowboy_req:set_resp_body(jiffy:encode(#{ success => false
                                                                          , error => reason_to_json(Reason)
                                                                          }), Req2),
                            io:format("Error checking password reset code: ~p~n", [Reason]),
                            { false, Res1, Session}
                    end;
                _ ->
                    { false, Req2, Session }
            end;
        false ->
            {false, Req, Session }
    end.

reason_to_json({Type, Subtype}) ->
    #{ type => Type
     , subtype => Subtype
     };
reason_to_json(Type) ->
    #{ type => Type
     }.
