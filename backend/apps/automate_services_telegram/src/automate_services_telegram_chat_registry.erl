-module(automate_services_telegram_chat_registry).

-export([start_link/0]).
-export([ get_prefix/0
        , count_chats/0
        , get_chats_for_user/1
        ]).

-include("../../automate_chat_registry/src/records.hrl").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    automate_chat_registry:register_prefix(get_prefix, automate_services_telegram_chat_registry).

-spec get_prefix() -> atom().
get_prefix() ->
    automate_telegram.

-spec count_chats() -> non_neg_integer().
count_chats() ->
    automate_services_telegram_storage:count_chats().

-spec get_chats_for_user(binary()) -> {ok, [#chat_entry{}]}.
get_chats_for_user(UserId) ->
    case automate_services_telegram_storage:get_telegram_id_from_userid(UserId) of
        { ok, TelegramId } ->
            automate_services_telegram_storage:get_chats_for_user(TelegramId);
        {error, not_found} ->
            {ok, []}
    end.
