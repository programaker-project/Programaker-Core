%%%% Instruction map entry names
-define(TYPE, <<"type">>).
-define(ARGUMENTS, <<"args">>).
-define(CONTENTS, <<"contents">>).
-define(ID, <<"id">>).
-define(VALUE, <<"value">>).

%%%% Command types
%% Triggers
-define(COMMAND_TELEGRAM_ON_RECEIVED_COMMAND, <<"chat_whenreceivecommand">>).

%% Operations
-define(COMMAND_CHAT_SAY, <<"chat_say">>).
-define(COMMAND_SET_VARIABLE, <<"data_setvariableto">>).
-define(COMMAND_CHANGE_VARIABLE, <<"data_changevariableby">>).
-define(COMMAND_WAIT, <<"control_wait">>).
-define(COMMAND_JOIN, <<"operation_join">>).
-define(COMMAND_REPEAT, <<"control_repeat">>).

%% Variables
-define(VARIABLE_BLOCK, <<"block">>).
-define(VARIABLE_CONSTANT, <<"constant">>).
-define(VARIABLE_VARIABLE, <<"variable">>).

%%%% Signal types
-define(SIGNAL_TELEGRAM_MESSAGE_RECEIVED, telegram_received_message).
-define(SIGNAL_PROGRAM_TICK, tick).

%%%% Operation parameters
-define(MILLIS_PER_TICK, 100).

%%%% Thread memory values
-define(TELEGRAM_CHAT_ID, telegram_chat_id).
-define(TELEGRAM_BOT_NAME, telegram_bot_name).
