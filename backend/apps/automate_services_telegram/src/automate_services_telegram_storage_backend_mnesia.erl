-module(automate_services_telegram_storage_backend_mnesia).

-define(TELEGRAM_SERVICE_REGISTRATION_TABLE, automate_telegram_service_registration_table).
-define(TELEGRAM_SERVICE_USER_CHANNEL_TABLE, automate_telegram_service_user_channel_table).
-define(TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, automate_telegram_service_chats_known_table).
-define(TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE, automate_telegram_service_chats_members_table).

-include("records.hrl").
-include("../../automate_chat_registry/src/records.hrl").
-include("../../automate_service_user_registration/src/records.hrl").

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

    ok = case mnesia:create_table(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE,
                                  [ { attributes, record_info(fields, telegram_service_known_chat_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, telegram_service_known_chat_entry }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,

    ok = case mnesia:create_table(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE,
                                  [ { attributes, record_info(fields, telegram_service_chat_member_entry)}
                                  , { disc_copies, Nodes }
                                  , { record_name, telegram_service_chat_member_entry }
                                  , { type, bag }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = mnesia:wait_for_tables([ ?TELEGRAM_SERVICE_REGISTRATION_TABLE
                                , ?TELEGRAM_SERVICE_USER_CHANNEL_TABLE
                                , ?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE
                                , ?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE
                                ], automate_configuration:get_table_wait_time()),
    ignore.

-spec get_internal_user_for_telegram_id(number()) -> {ok, binary()} | {error, not_found}.
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


-spec finish_telegram_registration(binary(), number()) -> ok | {error, not_found}.
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

-spec get_telegram_id_from_userid(binary()) -> {ok, number()} | {error, not_found}.
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

%% Chats
-spec count_chats() -> non_neg_integer().
count_chats() ->
    length(mnesia:dirty_all_keys(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE)).

-spec get_chats_for_user(number()) -> {ok, [#chat_entry{}]}.
get_chats_for_user(TelegramUserId) ->
    MatchHead = #telegram_service_chat_member_entry { user_id='$1'
                                                    , chat_id='$2'
                                                    },

    %% Check that neither the id, username or email matches another
    Guard = {'==', '$1', TelegramUserId},
    ResultColumn = '$2',
    ServiceMatcher = [{MatchHead, [Guard], [ResultColumn]}],

    Transaction = fun() ->
                          ChatIds = mnesia:select(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE, ServiceMatcher),
                          Chats = lists:flatmap(fun (ChatId) ->
                                                        mnesia:read(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, ChatId)
                                                end, ChatIds),
                          {ok, lists:map(fun translate_chat_metadata/1, Chats)}
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.


-spec register_chat(number(), binary()) -> {ok, new_chat | already_registered}.
register_chat(ChatId, ChatName) ->
    Id = integer_to_binary(ChatId),
    Transaction = fun() ->
                          case  mnesia:read(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, ChatId) of
                              [] ->
                                  ok = mnesia:write(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE,
                                                    #telegram_service_known_chat_entry{ chat_id=Id
                                                                                      , chat_name=ChatName
                                                                                      }, write),
                                  {ok, new_chat};
                              _ ->
                                  {ok, already_registered}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec unregister_chat(number()) -> {ok, unregistered_now | already_unregistered}.
unregister_chat(ChatId) ->
    Id = integer_to_binary(ChatId),
    Transaction = fun() ->
                          case mnesia:read(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, Id) of
                              [] ->
                                  {ok, already_unregistered};
                              _ ->
                                  %% Note that it's better do perform the select
                                  %% *BEFORE* doing changes to the table
                                  MatchHead = #telegram_service_chat_member_entry { user_id='_'
                                                                                  , chat_id='$1'
                                                                                  },

                                  %% Check that neither the id, username or email matches another
                                  Guard = {'==', '$1', Id},
                                  ResultColumn = '$_',
                                  ServiceMatcher = [{MatchHead, [Guard], [ResultColumn]}],

                                  Members = mnesia:select(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE, ServiceMatcher),
                                  lists:foreach(fun (Member) ->
                                                        ok = mnesia:delete_object(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE,
                                                                                  Member,
                                                                                  write)
                                                end, Members),

                                  ok = mnesia:delete(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, Id, write),
                                  {ok, unregistered_now}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec register_user_in_chat(number(), number()) -> {ok, registered_now | already_registered} | {error, chat_not_found}.
register_user_in_chat(NumChatId, UserId) ->
    ChatId = integer_to_binary(NumChatId),

    Transaction = fun() ->
                          case mnesia:read(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, ChatId) of
                              [] ->
                                  {error, chat_not_found};
                              _ ->
                                  MatchHead = #telegram_service_chat_member_entry { user_id='$1'
                                                                                  , chat_id='$2'
                                                                                  },

                                  %% Check that neither the id, username or email matches another
                                  Guard = {'andthen', {'==', '$1', UserId}, {'==', '$2', ChatId}},
                                  ResultColumn = '_',
                                  ServiceMatcher = [{MatchHead, [Guard], [ResultColumn]}],

                                  case mnesia:select(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE, ServiceMatcher) of
                                      [] ->
                                          ok = mnesia:write(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE,
                                                            #telegram_service_chat_member_entry{ user_id=UserId
                                                                                               , chat_id=ChatId
                                                                                               }, write),
                                          {ok, registered_now};
                                      _ ->
                                          {ok, already_registered}
                                  end
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.

-spec unregister_user_in_chat(number(), number()) -> {ok, unregistered_now | already_unregistered} | {error, chat_not_found}.
unregister_user_in_chat(NumChatId, UserId) ->
    ChatId = integer_to_binary(NumChatId),

    Transaction = fun() ->
                          case mnesia:read(?TELEGRAM_SERVICE_CHATS_KNOWN_TABLE, ChatId) of
                              [] ->
                                  {error, chat_not_found};
                              _ ->
                                  MatchHead = #telegram_service_chat_member_entry { user_id='$1'
                                                                                  , chat_id='$2'
                                                                                  },

                                  %% Check that neither the id, username or email matches another
                                  Guard = {'andthen', {'==', '$1', UserId}, {'==', '$2', ChatId}},
                                  ResultColumn = '_',
                                  ServiceMatcher = [{MatchHead, [Guard], [ResultColumn]}],

                                  case mnesia:select(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE, ServiceMatcher) of
                                      [] ->
                                          {ok, already_unregistered};
                                      Memberships ->
                                          lists:foreach(fun (Membership) ->
                                                                mnesia:delete_object(?TELEGRAM_SERVICE_CHATS_MEMBERS_TABLE,
                                                                                     Membership,
                                                                                     write)
                                                        end, Memberships)
                                  end
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, Reason, mnesia:error_description(Reason)}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
-spec translate_chat_metadata(#telegram_service_known_chat_entry{}) -> #chat_entry{}.
translate_chat_metadata(#telegram_service_known_chat_entry{ chat_id=Id
                                                          , chat_name=Name
                                                          }) ->
    #chat_entry{ chat_id={ automate_services_telegram_chat_registry:get_prefix()
                         , Id
                         }
               , chat_name=Name
               }.
