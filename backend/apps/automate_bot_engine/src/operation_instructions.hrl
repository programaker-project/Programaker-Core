%%%% Command types
%%%%  Operations
%% Call service
-define(COMMAND_CALL_SERVICE, <<"command_call_service">>).
-define(CONTEXT_SELECT_CONNECTION, <<"operator_select_connection">>).

%% General control
-define(COMMAND_WAIT, <<"control_wait">>).
-define(COMMAND_REPEAT, <<"control_repeat">>).
-define(COMMAND_REPEAT_UNTIL, <<"control_repeat_until">>).
-define(COMMAND_WAIT_UNTIL, <<"control_wait_until">>).
-define(COMMAND_IF, <<"control_if">>).
-define(COMMAND_IF_ELSE, <<"control_if_else">>).
-define(COMMAND_FORK_EXECUTION, <<"op_fork_execution">>).
-define(OP_FORK_CONTINUE_ON_FIRST, <<"exit-when-first-completed">> ).
-define(COMMAND_WAIT_FOR_NEXT_VALUE, <<"control_wait_for_next_value">> ).

%% String operations
-define(COMMAND_JOIN, <<"operator_join">>).
-define(COMMAND_STRING_CONTAINS, <<"operator_contains">>).
-define(COMMAND_JSON, <<"operator_json_parser">>).

%% Templates
-define(MATCH_TEMPLATE_STATEMENT, <<"automate_match_template_stmt">>).
-define(MATCH_TEMPLATE_CHECK, <<"automate_match_template_check">>).

%% Any() operations
-define(COMMAND_EQUALS, <<"operator_equals">>).
-define(COMMAND_LESS_THAN, <<"operator_lt">>).
-define(COMMAND_GREATER_THAN, <<"operator_gt">>).

%% Boolean operations
-define(COMMAND_AND, <<"operator_and">>).
-define(COMMAND_OR, <<"operator_or">>).
-define(COMMAND_NOT, <<"operator_not">>).

%% Numeric operations
-define(COMMAND_ADD, <<"operator_add">>).
-define(COMMAND_SUBTRACT, <<"operator_subtract">>).
-define(COMMAND_MULTIPLY, <<"operator_multiply">>).
-define(COMMAND_DIVIDE, <<"operator_divide">>).
-define(COMMAND_MODULO, <<"operator_modulo">>).

%% Variable control
-define(COMMAND_SET_VARIABLE, <<"data_setvariableto">>).
-define(COMMAND_CHANGE_VARIABLE, <<"data_changevariableby">>).
-define(COMMAND_DATA_VARIABLE, <<"data_variable">>).

%% List control
-define(COMMAND_ADD_TO_LIST, <<"data_addtolist">>).
-define(COMMAND_DELETE_OF_LIST, <<"data_deleteoflist">>).
-define(COMMAND_DELETE_ALL_LIST, <<"data_deletealloflist">>).
-define(COMMAND_INSERT_AT_LIST, <<"data_insertatlist">>).
-define(COMMAND_REPLACE_VALUE_AT_INDEX, <<"data_replaceitemoflist">>).
-define(COMMAND_ITEM_OF_LIST, <<"data_itemoflist">>).
-define(COMMAND_ITEMNUM_OF_LIST, <<"data_itemnumoflist">>).
-define(COMMAND_LENGTH_OF_LIST, <<"data_lengthoflist">>).
-define(COMMAND_LIST_CONTAINS_ITEM, <<"data_listcontainsitem">>).
-define(COMMAND_LIST_GET_CONTENTS, <<"data_listcontents">>).

%% Data-control operations
%% Introduced by compiler
-define(FLOW_LAST_VALUE, <<"flow_last_value">>).
-define(COMMAND_PRELOAD_GETTER, <<"op_preload_getter">>).

%% Debugging
-define(COMMAND_LOG_VALUE, <<"logging_add_log">>).
-define(COMMAND_GET_THREAD_ID, <<"flow_get_thread_id">>).
