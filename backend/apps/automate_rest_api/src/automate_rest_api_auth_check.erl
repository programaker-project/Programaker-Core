%%% @doc
%%% REST authorization evaluation.
%%% @end

-module(automate_rest_api_auth_check).
-export([is_authorized/2]).
-include("./records.hrl").

-spec is_authorized(cowboy_req:req(),_) -> {'true' | {'false', binary()}, cowboy_req:req(),_}.
is_authorized(Req, State) ->
    Cookie = cowboy_req:parse_header(<<"cookie">>, Req),
    case get_token_from_cookie(Cookie) of
        none ->
            {{false, <<"Cookie">>}, Req, State};
        {token, Token} ->
            case session_manager:try_get_session({api, Token}) of
                {ok, SessionId} ->
                    {true, Req, #rest_session{user_id={api, Token},
                                              session_id=SessionId}};
                {error, session_not_exists} ->
                    {{false, <<"Cookie">>}, Req, State}
            end
    end.


-spec get_token_from_cookie(maybe_improper_list()) -> 'none' | {'token', binary()}.
get_token_from_cookie([]) ->
    none;

get_token_from_cookie([{<<"TOKEN">>, Token} | _]) ->
    {token, Token};

get_token_from_cookie([_ | T]) ->
    get_token_from_cookie(T).
