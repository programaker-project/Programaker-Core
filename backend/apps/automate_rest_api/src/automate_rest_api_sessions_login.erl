%%% @doc
%%% REST endpoint to manage knowledge collections.
%%% @end

-module(automate_rest_api_sessions_login).
-export([init/2]).
-export([ allowed_methods/2
        , content_types_accepted/2
        , options/2
        ]).

-export([accept_json_modify_collection/2]).

-define(UTILS, automate_rest_api_utils).
-include("./records.hrl").

-record(login_seq, { rest_session :: binary() | undefined,
                     login_data
                   }).

-spec init(_,_) -> {'cowboy_rest',_,_}.
init(Req, _Opts) ->
    {cowboy_rest, Req
    , #login_seq{ rest_session=undefined
                , login_data=undefined}}.

%% -spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
%% is_authorized(Req, State) ->
%%     rest_is_authorized:is_authorized(Req, State).

%% CORS
options(Req, State) ->
    Res = automate_rest_api_cors:set_headers(Req),
    {ok, Res, State}.

-spec allowed_methods(cowboy_req:req(),_) -> {[binary()], cowboy_req:req(),_}.
allowed_methods(Req, State) ->
    Res = automate_rest_api_cors:set_headers(Req),
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
            Parsed = [jiffy:decode(Body, [return_maps])],
            case to_register_data(Parsed) of
                { ok, LoginData } ->
                    case automate_rest_api_backend:login_user(LoginData) of
                        { ok, {Token, UserId} } ->
                            Output = jiffy:encode(#{ <<"token">> => Token
                                                   , <<"success">> => true
                                                   , <<"user_id">> => UserId
                                                   }),
                            Res1 = cowboy_req:set_resp_body(Output, Req2),
                            Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
                            Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
                            { true, Res3, Session#login_seq{
                                            login_data=LoginData} };

                        {error, Reason} ->
                            Res1 = cowboy_req:set_resp_body(jiffy:encode(#{ success => false
                                                                          , error => reason_to_json(Reason)
                                                                          }), Req2),
                            automate_logging:log_api(error, ?MODULE, Reason),
                            { false, Res1, Session}
                    end;
                { error, _Reason } ->
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


to_register_data([#{ <<"password">> := Password
                   , <<"username">> := Username
                   }]) ->
    { ok, #login_rec{ password=Password
                    , username=Username
                    } };

to_register_data(_X) ->
    { error, "Data structures not matching" }.
