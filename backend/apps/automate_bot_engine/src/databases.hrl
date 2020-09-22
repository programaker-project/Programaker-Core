-include("../../automate_storage/src/databases.hrl").

-define(BOT_REQUIRED_DBS, [ ?USER_PROGRAMS_TABLE
                          , ?RUNNING_PROGRAMS_TABLE
                          , ?RUNNING_THREADS_TABLE
                          , ?PROGRAM_VARIABLE_TABLE
                          ]).
-define(BOT_EXTRA_DBS, [ ?PROGRAM_TAGS_TABLE
                       , ?USER_PROGRAM_LOGS_TABLE
                       , ?USER_GENERATED_LOGS_TABLE
                       , ?USER_PROGRAM_EVENTS_TABLE
                       , ?USER_PROGRAM_CHECKPOINTS_TABLE
                       , ?CUSTOM_SIGNALS_TABLE
                       ]).
