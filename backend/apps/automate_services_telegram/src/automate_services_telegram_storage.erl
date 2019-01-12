-module(automate_services_telegram_storage).

-define(BACKEND, automate_services_telegram_storage_backend_mnesia).

-export([ start_link/0
        , get_internal_user_for_telegram_id/1
        , get_telegram_id_from_userid/1
        , finish_telegram_registration/2
        , user_has_registered/1

        , get_or_gen_user_channel/1
        , count_chats/0
        , get_chats_for_user/1

        , register_chat/2
        , unregister_chat/1
        , register_user_in_chat/2
        , unregister_user_in_chat/2
        ]).

-include("../../automate_chat_registry/src/records.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    ?BACKEND:start_link().

-spec get_internal_user_for_telegram_id(number()) -> {ok, binary()} | {error, not_found}.
get_internal_user_for_telegram_id(TelegramId) ->
    ?BACKEND:get_internal_user_for_telegram_id(TelegramId).

-spec get_telegram_id_from_userid(binary()) -> {ok, number()} | {error, not_found}.
get_telegram_id_from_userid(InternalUser) ->
    ?BACKEND:get_telegram_id_from_userid(InternalUser).

-spec finish_telegram_registration(number(), binary()) -> ok | {error, not_found}.
finish_telegram_registration(TelegramUserId, RegistrationToken) ->
    ?BACKEND:finish_telegram_registration(RegistrationToken, TelegramUserId).

-spec user_has_registered(binary()) -> {ok, boolean()}.
user_has_registered(Username) ->
    ?BACKEND:user_has_registered(Username).

-spec get_or_gen_user_channel(binary()) -> {ok, binary()}.
get_or_gen_user_channel(UserId) ->
    ?BACKEND:get_or_gen_user_channel(UserId).

%% Chats
-spec count_chats() -> non_neg_integer().
count_chats() ->
    ?BACKEND:count_chats().

-spec get_chats_for_user(number()) -> {ok, [#chat_entry{}]}.
get_chats_for_user(TelegramUserId) ->
    ?BACKEND:get_chats_for_user(TelegramUserId).

-spec register_chat(number(), binary()) -> {ok, new_chat | already_registered}.
register_chat(ChatId, ChatName) ->
    ?BACKEND:register_chat(ChatId, ChatName).

-spec unregister_chat(number()) -> {ok, unregistered_now | already_unregistered}.
unregister_chat(ChatId) ->
    ?BACKEND:unregister_chat(ChatId).

-spec register_user_in_chat(number(), number()) -> {ok, registered_now | already_registered} | {error, chat_not_found}.
register_user_in_chat(ChatId, UserId) ->
    ?BACKEND:register_user_in_chat(ChatId, UserId).

-spec unregister_user_in_chat(number(), number()) -> {ok, unregistered_now | already_unregistered} | {error, chat_not_found}.
unregister_user_in_chat(ChatId, TelegramUserId) ->
    ?BACKEND:unregister_user_in_chat(ChatId, TelegramUserId).
