-define(JUST_WAIT_PROGRAM_ID, <<"65c4a6f2-5f1b-47fc-b7cc-0bce1fe3efa1">>).
-define(JUST_WAIT_PROGRAM_USER_ID, <<"1ee0f163-2b44-47e6-bec3-8e6db65d8e52">>).
-define(JUST_WAIT_PROGRAM_NAME, <<"74383216-2742-4b10-809b-7186d98211df">>).
-define(JUST_WAIT_PROGRAM_TYPE, <<"scratch_program">>).
-define(JUST_WAIT_MONITOR_ID, <<"__just_wait_monitor_id__">>).

%% WaitForMonitorInstruction = #{ ?TYPE => ?WAIT_FOR_MONITOR
%%                              , ?ARGUMENTS => #{ ?MONITOR_ID => ?TEST_MONITOR
%%                                               , ?MONITOR_EXPECTED_VALUE => #{ ?TYPE => ?VARIABLE_CONSTANT
%%                                                                             , ?VALUE => example
%%                                                                             }
%%                                               }
%%                              },

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

-define(JUST_WAIT_PROGRAM,
        #user_program_entry{ id=?JUST_WAIT_PROGRAM_ID
                           , user_id=?JUST_WAIT_PROGRAM_USER_ID
                           , program_name=?JUST_WAIT_PROGRAM_NAME
                           , program_type=?JUST_WAIT_PROGRAM_TYPE
                           , program_parsed=#{ <<"blocks">> => [[ ?JUST_WAIT_PROGRAM_TRIGGER
                                                                  | ?JUST_WAIT_PROGRAM_INSTRUCTIONS ]]

                                             , <<"variables">> => ?JUST_WAIT_PROGRAM_VARIABLES
                                             }
                           , program_orig=?JUST_WAIT_PROGRAM_ORIG
                           }).


-define(JUST_WAIT_PROGRAM_INITIALIZATION,
        #program_state{ program_id=?JUST_WAIT_PROGRAM_ID
                      , variables=?JUST_WAIT_PROGRAM_VARIABLES
                      , triggers=[#program_trigger{ condition=?JUST_WAIT_PROGRAM_TRIGGER
                                                  , subprogram=?JUST_WAIT_PROGRAM_INSTRUCTIONS
                                                  }
                                 ]
                      }).
