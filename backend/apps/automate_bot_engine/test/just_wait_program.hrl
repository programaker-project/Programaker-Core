-define(JUST_WAIT_PROGRAM_TYPE, <<"scratch_program">>).
-define(JUST_WAIT_MONITOR_ID, <<"__just_wait_monitor_id__">>).

-define(JUST_WAIT_PROGRAM_INSTRUCTIONS, [#{<<"args">> => [#{<<"type">> => <<"constant">>,
                                                          <<"value">> => <<"10">>}]
                                          , <<"contents">> => []
                                          , <<"type">> => <<"control_wait">>
                                          }]).

-define(JUST_WAIT_PROGRAM_VARIABLES, []).
-define(JUST_WAIT_PROGRAM_ORIG, <<"">>).
