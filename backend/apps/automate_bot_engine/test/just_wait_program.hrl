-define(JUST_WAIT_PROGRAM_TYPE, <<"scratch_program">>).
-define(JUST_WAIT_MONITOR_ID, <<"__just_wait_monitor_id__">>).

-define(JUST_WAIT_PROGRAM_TRIGGER, #{ ?ARGUMENTS =>
                                         #{ ?MONITOR_ID => ?JUST_WAIT_MONITOR_ID
                                          , ?MONITOR_EXPECTED_VALUE => #{ ?TYPE => ?VARIABLE_CONSTANT
                                                                        , ?VALUE => start
                                                                        }
                                          }
                                    , ?TYPE => ?WAIT_FOR_MONITOR}).

-define(JUST_WAIT_PROGRAM_INSTRUCTIONS, [#{<<"args">> => [#{<<"type">> => <<"constant">>,
                                                          <<"value">> => <<"10">>}]
                                          , <<"contents">> => []
                                          , <<"type">> => <<"control_wait">>
                                          }]).

-define(JUST_WAIT_PROGRAM_VARIABLES, []).
-define(JUST_WAIT_PROGRAM_ORIG, <<"">>).

-define(JUST_WAIT_PROGRAM_INITIALIZATION,
        #program_state{ program_id=?JUST_WAIT_PROGRAM_ID
                      , variables=?JUST_WAIT_PROGRAM_VARIABLES
                      , triggers=[#program_trigger{ condition=?JUST_WAIT_PROGRAM_TRIGGER
                                                  , subprogram=?JUST_WAIT_PROGRAM_INSTRUCTIONS
                                                  }
                                 ]
                      }).
