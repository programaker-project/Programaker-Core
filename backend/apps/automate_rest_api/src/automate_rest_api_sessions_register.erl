%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_sessions_register).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_accepted/2
        , options/2
        ]).
-export([resource_exists/2]).

-export([accept_json_modify_collection/2]).

-define(UTILS, automate_rest_api_utils).
-define(FORMAT, automate_rest_api_utils_formatting).
-include("./records.hrl").

-record(state, {}).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    Res = automate_rest_api_cors:set_headers(Req),
    {cowboy_rest, Res
    , #state{}}.

resource_exists(Req, State) ->
    {false, Req, State}.

%% -spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
%% is_authorized(Req, State) ->
%%     rest_is_authorized:is_authorized(Req, State).

%% CORS
options(Req, State) ->
    {ok, Req, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, accept_json_modify_collection}],
     Req, State}.

%%%% POST
                                                %
-spec accept_json_modify_collection(cowboy_req:req(),#state{})
                                   -> {'false' | {'true', binary()},cowboy_req:req(),#state{}}.
accept_json_modify_collection(Req, Session) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, Req2} = ?UTILS:read_body(Req),
            Parsed = [jiffy:decode(Body, [return_maps])],
            case to_register_data(Parsed) of
                { ok, RegistrationData } ->
                    case automate_rest_api_backend:register_user(RegistrationData) of
                        { ok, NextStatus } ->

                            Output = case NextStatus of
                                         continue ->
                                             jiffy:encode(#{ success => true, ready => true });
                                         wait_for_mail_verification ->
                                             jiffy:encode(#{ success => true, ready => false })
                                     end,
                            Res1 = cowboy_req:set_resp_body(Output, Req2),
                            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
                            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
                            { true, Res3, Session};
                        {error, Reason} ->
                            Res1 = ?UTILS:send_json_output(jiffy:encode(#{ success => false
                                                                         , error => ?FORMAT:reason_to_json(Reason)
                                                                         }), Req2),
                            {false, Res1, Session}
                    end;
                { error, _Reason } ->
                    { false, Req2, Session }
            end;
        false ->
            {false, Req, Session }
    end.

to_register_data([#{ <<"email">> := Email
                   , <<"password">> := Password
                   , <<"username">> := Username
                   }]) ->
    { ok, #registration_rec{ password=Password
                           , username=Username
                           , email=Email
                           } };

to_register_data(_) ->
    { error, "Data structures not matching" }.
