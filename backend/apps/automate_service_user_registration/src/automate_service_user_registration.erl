-module(automate_service_user_registration).

-define(BACKEND, automate_service_user_registration_backend_mnesia).

-export([ start_link/0
        , get_info_from_registration_token/1
        , get_or_gen_registration_token/2
        ]).


-include("records.hrl").
%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    ignore = ?BACKEND:start_link(),
    {ok, self()}.

-spec get_info_from_registration_token(binary()) -> {ok, #service_registration_token{}} | {error, not_found}.
get_info_from_registration_token(Token) ->
    ?BACKEND:get_info_from_registration_token(Token).

-spec get_or_gen_registration_token(binary(), binary()) -> {ok, binary()}.
get_or_gen_registration_token(Username, ServiceId) ->
    case ?BACKEND:get_registration_token(Username, ServiceId) of
        {ok, Token} ->
            {ok, Token};
        {error, not_found} ->
            case ?BACKEND:gen_registration_token(Username, ServiceId) of
                {ok, Token} ->
                    {ok, Token}
            end
    end.
