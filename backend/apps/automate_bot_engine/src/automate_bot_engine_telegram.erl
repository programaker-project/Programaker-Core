%%%-------------------------------------------------------------------
%% @doc automate_bot_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_telegram).

%% API
-export([ is_enabled/0
        , user_has_enabled_platform/1
        , get_platform_id/0
        , get_platform_name/0
        , get_bot_name/0
        , get_registration_token/1
        , telegram_user_to_internal/1
        , register_user/2
        , send_message/2
        ]).

-define(APPLICATION, automate_bot_engine).

%%====================================================================
%% API functions
%%====================================================================

is_enabled() ->
    {ok, Enabled} = application:get_env(?APPLICATION, telegram_enabled),
    Enabled.

get_platform_name() ->
    <<"Telegram">>.

get_platform_id() ->
    <<"__telegram-platform-im-bot">>.


-spec send_message(binary(), map()) -> {ok, _}.
-ifdef(TEST).
send_message(BotName, Params) ->
    {ok, ignored_on_testing}.
-else.
send_message(BotName, Params) ->
    try pe4kin:send_message(BotName, Params) of
        {ok, Response} ->
            {ok, Response}
    catch X:Y ->
            io:format("Error sending message: ~p~n", [{error, {X, Y}}]),
            {ok, error_ignored}
    end.
-endif.


-spec user_has_enabled_platform(binary()) -> {'ok', 'true' | 'false'} | {error, any()}.
user_has_enabled_platform(Username) ->
    automate_storage:user_has_registered_service(Username, get_platform_id()).


-spec get_bot_name() -> binary().
get_bot_name() ->
    {ok, BotName} = application:get_env(?APPLICATION, telegram_bot_name),
    BotName.


-spec get_registration_token(binary()) -> {ok, binary()}.
get_registration_token(Username) ->
    automate_storage:get_or_gen_registration_token(Username, get_platform_id()).


-spec telegram_user_to_internal(binary()) -> {ok, binary()} | {error, not_found}.
telegram_user_to_internal(TelegramId) ->
    automate_storage:get_internal_user_for_telegram_id(TelegramId).

register_user(TelegramUserId, RegistrationToken) ->
    automate_storage:finish_telegram_registration(TelegramUserId, RegistrationToken).
