%%%-------------------------------------------------------------------
%% @doc automate_services_telegram top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_telegram).

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

%% Service API
-export([ start_link/0
        , get_description/0
        , get_actions/0
        , get_uuid/0
        , get_name/0
        , is_enabled_for_user/1
        , get_how_to_enable/1
        ]).


-define(APPLICATION, automate_services_telegram).


-record(service_metadata, { id
                          , name
                          , link
                          , enabled
                          }).

-record(service_enable_extra_telegram, { token :: binary()
                                       , bot_name :: binary()
                                       }).

-record(service_enable_how_to, { service :: #service_metadata{}
                               , method :: 'external'
                               , extra :: #service_enable_extra_telegram{}
                               }).

%%====================================================================
%% API functions
%%====================================================================

is_enabled() ->
    io:format("==> ~p~n", [application:get_env(?APPLICATION)]),
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


%%====================================================================
%% Service API
%%====================================================================
start_link() ->
    ignore.

get_uuid() ->
    <<"c8062378-9b53-4962-b4f4-e5a71e34d335">>.

get_name() ->
    <<"Telegram">>.

get_description() ->
    <<"Global telegram service.">>.

get_actions() ->
    [
    ].

is_enabled_for_user(Username) ->
    user_has_enabled_platform(Username).

get_how_to_enable(#{ user_name := Username }) ->
    {ok, RegistrationToken} = get_registration_token(Username),
    BotName = get_bot_name(),
    case get_telegram_services_from_username(Username) of
        [ Service | _] ->
            {ok, #service_enable_how_to{ service=Service
                                       , method='external'
                                       , extra=#service_enable_extra_telegram{ token=RegistrationToken
                                                                             , bot_name=BotName
                                                                             }
                                       }}
    end.

%%====================================================================
%% Auxiliary functions
%%====================================================================
-spec generate_url_from_service(binary(), binary()) -> binary().
generate_url_from_service(Username, ServiceId) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/services/~s", [Username, ServiceId]))).

-spec get_telegram_services_from_username(binary()) -> [ #service_metadata{} ].
get_telegram_services_from_username(Username) ->
    DefaultId = get_platform_id(),
    DefaultName = get_platform_name(),
    case is_enabled() of
        true ->
            {ok, HasEnabled} = user_has_enabled_platform(Username),
            [ #service_metadata{ id=DefaultId
                               , name=DefaultName
                               , link=generate_url_from_service(Username, DefaultId)
                               , enabled=HasEnabled
                               } ];
        false ->
            []
    end.
