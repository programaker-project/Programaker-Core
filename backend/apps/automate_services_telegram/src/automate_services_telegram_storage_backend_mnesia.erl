-module(automate_services_telegram_storage_backend_mnesia).

-define(TELEGRAM_SERVICE_REGISTRATION_TABLE, automate_telegram_service_registration_table).
-define(TELEGRAM_SERVICE_USER_CHANNEL_TABLE, automate_telegram_service_user_channel_table).

-record(telegram_service_registration_entry, { telegram_user_id :: binary() | '_' | '$1' | '$2'
                                             , internal_user_id :: binary() | '_' | '$1' | '$2'
                                             }).

-record(telegram_service_user_channel_entry, { internal_user_id :: binary() | '_' | '$1' | '$2'
                                             , channel_id :: binary() | '_' | '$1' | '$2'
                                             }).

-include("../../automate_service_user_registration/src/records.hrl").

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
    Nodes = [node()],

    %% TelegramId -> InternalId matches
    ok = case mnesia:create_table(?TELEGRAM_SERVICE_REGISTRATION_TABLE,
                                  [ { attributes, record_info(fields, telegram_service_registration_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, telegram_service_registration_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    ok = case mnesia:create_table(?TELEGRAM_SERVICE_USER_CHANNEL_TABLE,
                                  [ { attributes, record_info(fields, telegram_service_user_channel_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, telegram_service_user_channel_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ignore.


-spec get_internal_user_for_telegram_id(binary()) -> {ok, binary()} | {error, not_found}.
get_internal_user_for_telegram_id(TelegramId) ->
    Transaction = fun() ->
                          mnesia:read(?TELEGRAM_SERVICE_REGISTRATION_TABLE, TelegramId)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [#telegram_service_registration_entry{internal_user_id=UserId}] } ->
            {ok, UserId};
        { atomic, [] } ->
            {error, not_found};
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, mnesia:error_description(Reason)}
    end.


-spec finish_telegram_registration(binary(), binary()) -> ok | {error, not_found}.
finish_telegram_registration(RegistrationToken, TelegramUserId) ->
    Transaction = fun() ->
                          case automate_service_user_registration:get_info_from_registration_token(RegistrationToken) of
                              {ok, #service_registration_token{ service_id=ServiceId
                                                              , user_id=UserId
                                                              }} ->
                                  case automate_services_telegram:get_platform_id() of
                                      ServiceId ->
                                          ok = mnesia:write(?TELEGRAM_SERVICE_REGISTRATION_TABLE,
                                                            #telegram_service_registration_entry{ telegram_user_id=TelegramUserId
                                                                                                , internal_user_id=UserId
                                                                                                }, write);
                                      _ ->
                                          %% TODO log appropiately (matched with token from another service)
                                          io:format("[Error] Matched token on another service~n"),
                                          {error, not_found}
                                  end;
                              {error, not_found} ->
                                  io:format("[Error] No token match~n"),
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.


-spec user_has_registered(binary()) -> {ok, boolean()}.
user_has_registered(Username) ->
    {ok, UserId} = automate_storage:get_userid_from_username(Username),

    case get_telegram_id_from_userid(UserId) of
        {ok, _} ->
            {ok, true};
        {error, not_found} ->
            {ok, false}
    end.

-spec get_telegram_id_from_userid(binary()) -> {ok, binary()} | {error, not_found}.
get_telegram_id_from_userid(UserId) ->
    MatchHead = #telegram_service_registration_entry { telegram_user_id='$1'
                                                     , internal_user_id='$2'
                                                     },

    %% Check that neither the id, username or email matches another
    Guard = {'==', '$2', UserId},
    ResultColumn = '$1',
    ServiceMatcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?TELEGRAM_SERVICE_REGISTRATION_TABLE, ServiceMatcher) of
                              [TelegramUserId] ->
                                  {ok, TelegramUserId};
                              [] ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec get_or_gen_user_channel(binary()) -> {ok, binary()}.
get_or_gen_user_channel(UserId) ->
    MatchHead = #telegram_service_user_channel_entry { internal_user_id='$1'
                                                     , channel_id='$2'
                                                     },

    %% Check that neither the id, username or email matches another
    Guard = {'==', '$1', UserId},
    ResultColumn = '$2',
    ServiceMatcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          case mnesia:select(?TELEGRAM_SERVICE_USER_CHANNEL_TABLE, ServiceMatcher) of
                              [ChannelId] ->
                                  {ok, ChannelId};
                              [] ->
                                  {ok, ChannelId} = automate_channel_engine:create_channel(),
                                  ok = mnesia:write(?TELEGRAM_SERVICE_USER_CHANNEL_TABLE,
                                                    #telegram_service_user_channel_entry{ internal_user_id=UserId
                                                                                        , channel_id=ChannelId
                                                                                        }, write),
                                  {ok, ChannelId}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

