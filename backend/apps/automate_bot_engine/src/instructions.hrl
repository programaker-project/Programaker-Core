%%%% Instruction map entry names
-define(TYPE, <<"type">>).
-define(ARGUMENTS, <<"args">>).
-define(CONTENTS, <<"contents">>).
-define(ID, <<"id">>).
-define(VALUE, <<"value">>).

%%%% Command types
%% Triggers
-define(COMMAND_TELEGRAM_ON_RECEIVED_COMMAND, <<"chat_whenreceivecommand">>).

%%%%  Operations
%% Chat
-define(COMMAND_CHAT_SAY, <<"chat_say">>).

%% General control
-define(COMMAND_WAIT, <<"control_wait">>).
-define(COMMAND_REPEAT, <<"control_repeat">>).

%% String operations
-define(COMMAND_JOIN, <<"operator_join">>).

%% Variable control
-define(COMMAND_SET_VARIABLE, <<"data_setvariableto">>).
-define(COMMAND_CHANGE_VARIABLE, <<"data_changevariableby">>).
-define(COMMAND_DATA_VARIABLE, <<"data_variable">>).

%% List control
-define(COMMAND_ADD_TO_LIST, <<"data_addtolist">>).
-define(COMMAND_DELETE_OF_LIST, <<"data_deleteoflist">>).
-define(COMMAND_INSERT_AT_LIST, <<"data_insertatlist">>).
-define(COMMAND_REPLACE_VALUE_AT_INDEX, <<"data_replaceitemoflist">>).
-define(COMMAND_ITEM_OF_LIST, <<"data_itemoflist">>).
-define(COMMAND_ITEMNUM_OF_LIST, <<"data_itemnumoflist">>).
-define(COMMAND_LENGTH_OF_LIST, <<"data_lengthoflist">>).
-define(COMMAND_LIST_CONTAINS_ITEM, <<"data_listcontainsitem">>).

%%%% Variables
-define(VARIABLE_BLOCK, <<"block">>).
-define(VARIABLE_CONSTANT, <<"constant">>).
-define(VARIABLE_VARIABLE, <<"variable">>).
-define(VARIABLE_LIST, <<"list">>).

%%%% Signal types
-define(SIGNAL_TELEGRAM_MESSAGE_RECEIVED, telegram_received_message).
-define(SIGNAL_PROGRAM_TICK, tick).

%%%% Operation parameters
-define(MILLIS_PER_TICK, 100).

%%%% Thread memory values
-define(TELEGRAM_CHAT_ID, telegram_chat_id).
-define(TELEGRAM_BOT_NAME, telegram_bot_name).
