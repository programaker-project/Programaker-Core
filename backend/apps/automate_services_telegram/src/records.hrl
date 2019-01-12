-include("../../automate_channel_engine/src/records.hrl").

-define(TELEGRAM_MESSAGE_CONTENT, ?CHANNEL_MESSAGE_CONTENT).
-define(TELEGRAM_MESSAGE_CHAT_ID, <<"telegram_chat_id">>).
-define(TELEGRAM_MESSAGE_BOT_NAME, <<"telegram_bot_name">>).

-define(MNESIA_SELECTOR,  '_' | '$1' | '$2' | '$3').

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


-record(telegram_service_registration_entry, { telegram_user_id :: number() | ?MNESIA_SELECTOR
                                             , internal_user_id :: binary() | ?MNESIA_SELECTOR
                                             }).

-record(telegram_service_user_channel_entry, { internal_user_id :: binary() | ?MNESIA_SELECTOR
                                             , channel_id       :: binary() | ?MNESIA_SELECTOR
                                             }).

-record(telegram_service_known_chat_entry, { chat_id   :: binary() | ?MNESIA_SELECTOR
                                           , chat_name :: binary() | ?MNESIA_SELECTOR
                                           }).


-record(telegram_service_chat_member_entry, { user_id :: binary() | ?MNESIA_SELECTOR
                                            , chat_id :: binary() | ?MNESIA_SELECTOR
                                            }).


