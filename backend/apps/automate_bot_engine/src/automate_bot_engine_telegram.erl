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
