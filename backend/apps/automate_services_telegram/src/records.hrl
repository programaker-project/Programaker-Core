-include("../../automate_channel_engine/src/records.hrl").

-define(TELEGRAM_MESSAGE_CONTENT, ?CHANNEL_MESSAGE_CONTENT).
-define(TELEGRAM_MESSAGE_CHAT_ID, <<"telegram_chat_id">>).
-define(TELEGRAM_MESSAGE_BOT_NAME, <<"telegram_bot_name">>).



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
