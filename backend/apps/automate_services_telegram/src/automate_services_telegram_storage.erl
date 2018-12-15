-module(automate_services_telegram_storage).

-define(BACKEND, automate_services_telegram_storage_backend_mnesia).

-export([ start_link/0
        , get_internal_user_for_telegram_id/1
        , finish_telegram_registration/2
        , user_has_registered/1

        , get_or_gen_user_channel/1
        ]).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    ?BACKEND:start_link().

-spec get_internal_user_for_telegram_id(binary()) -> {ok, binary()} | {error, not_found}.
get_internal_user_for_telegram_id(TelegramId) ->
    ?BACKEND:get_internal_user_for_telegram_id(TelegramId).

-spec finish_telegram_registration(binary(), binary()) -> ok | {error, not_found}.
finish_telegram_registration(TelegramUserId, RegistrationToken) ->
    ?BACKEND:finish_telegram_registration(RegistrationToken, TelegramUserId).

-spec user_has_registered(binary()) -> {ok, boolean()}.
user_has_registered(Username) ->
    ?BACKEND:user_has_registered(Username).

-spec get_or_gen_user_channel(binary()) -> {ok, binary()}.
get_or_gen_user_channel(UserId) ->
    ?BACKEND:get_or_gen_user_channel(UserId).
