-define(SINGLE_LINE_PROGRAM_ID, <<"20ea6a52-4fa8-4385-abbc-c543b1103509">>).
-define(SINGLE_LINE_PROGRAM_USER_ID, <<"5cef743e-bc07-4a37-8e22-de055032ec25">>).
-define(SINGLE_LINE_PROGRAM_NAME, <<"5a07173d-0180-45d5-b232-6b5427ac011b">>).
-define(SINGLE_LINE_PROGRAM_TYPE, <<"scratch_program">>).

-define(SINGLE_LINE_PROGRAM_TRIGGER, #{<<"args">> =>
                                           [#{<<"type">> => <<"constant">>,
                                              <<"value">> => <<"/start">>}],
                                       <<"contents">> => [],
                                       <<"id">> => <<"4_Y1w,UJ5G_L2*_S(M;)">>,
                                       <<"type">> => <<"chat_whenreceivecommand">>}).

-define(SINGLE_LINE_PROGRAM_INSTRUCTIONS, [#{<<"args">> =>
                                                 [#{<<"type">> => <<"constant">>,
                                                    <<"value">> => <<"Hello!">>}],
                                             <<"contents">> => [],
                                             <<"id">> => <<"l;?R,0E%ti`V/9z{aHe,">>,
                                             <<"type">> => <<"chat_say">>},
                                           #{<<"args">> =>
                                                 [#{<<"type">> => <<"variable">>,
                                                    <<"value">> => <<"count">>},
                                                  #{<<"type">> => <<"constant">>,
                                                    <<"value">> => <<"0">>}],
                                             <<"contents">> => [],
                                             <<"id">> => <<"39bxL2I$88|Pqf!v~n[t">>,
                                             <<"type">> => <<"data_setvariableto">>},
                                           #{<<"args">> =>
                                                 [#{<<"type">> => <<"constant">>,
                                                    <<"value">> => <<"3">>}],
                                             <<"contents">> =>
                                                 [#{<<"args">> =>
                                                        [#{<<"type">> => <<"constant">>,
                                                           <<"value">> => <<"1">>}],
                                                    <<"contents">> => [],
                                                    <<"id">> => <<"!TKm=kc/kiDz1L|3!}x9">>,
                                                    <<"type">> => <<"control_wait">>},
                                                  #{<<"args">> =>
                                                        [#{<<"type">> => <<"variable">>,
                                                           <<"value">> => <<"count">>},
                                                         #{<<"type">> => <<"constant">>,
                                                           <<"value">> => <<"1">>}],
                                                    <<"contents">> => [],
                                                    <<"id">> => <<"i,/S]2-SY5HkVt5Y*VbA">>,
                                                    <<"type">> =>
                                                        <<"data_changevariableby">>},
                                                  #{<<"args">> =>
                                                        [#{<<"type">> => <<"block">>,
                                                           <<"value">> =>
                                                               [#{<<"args">> =>
                                                                      [#{<<"type">> =>
                                                                             <<"constant">>,
                                                                         <<"value">> =>
                                                                             <<"Counted to ">>},
                                                                       #{<<"type">> =>
                                                                             <<"block">>,
                                                                         <<"value">> =>
                                                                             [#{<<"args">> =>
                                                                                    [#{<<"type">> =>
                                                                                           <<"variable">>,
                                                                                       <<"value">> =>
                                                                                           <<"count">>}],
                                                                                <<"contents">> =>
                                                                                    [],
                                                                                <<"id">> =>
                                                                                    <<"^jsCxw}+j98X!,JA=xTV">>,
                                                                                <<"type">> =>
                                                                                    <<"data_variable">>}]}],
                                                                  <<"contents">> => [],
                                                                  <<"id">> =>
                                                                      <<"FGPL0wp@pECda4C(?=3t">>,
                                                                  <<"type">> =>
                                                                      <<"operator_join">>}]}],
                                                    <<"contents">> => [],
                                                    <<"id">> => <<"^#ie|?!BUAKR6w6{9gmS">>,
                                                    <<"type">> => <<"chat_say">>}],
                                             <<"id">> => <<"VfE`?a4`c4b?^D8rc0`y">>,
                                             <<"type">> => <<"control_repeat">>},
                                           #{<<"args">> =>
                                                 [#{<<"type">> => <<"constant">>,
                                                    <<"value">> => <<"See?">>}],
                                             <<"contents">> => [],
                                             <<"id">> => <<"]GTkQkA]]~KdJ?(G$kj2">>,
                                             <<"type">> => <<"chat_say">>}]).

-define(SINGLE_LINE_PROGRAM_VARIABLES, [#{<<"id">> => <<"H42ZX:dR5{Bwui#r]te+">>,
                                          <<"name">> => <<"count">>,
                                          <<"type">> => #{}},
                                        #{<<"id">> => <<"aJnIkMJmtrYSl$VuaJd_">>,
                                          <<"name">> => <<"compra">>,
                                          <<"type">> => #{}}]).

-define(SINGLE_LINE_PROGRAM_ORIG, <<"<xml xmlns=\"http://www.w3.org/1999/xhtml\">\n  <variables>\n    <variable type=\"\" id=\"H42ZX:dR5{Bwui#r]te+\">count</variable>\n    <variable type=\"list\" id=\"aJnIkMJmtrYSl$VuaJd_\">compra</variable>\n  </variables>\n  <block type=\"chat_whenreceivecommand\" id=\"4_Y1w,UJ5G_L2*_S(M;)\" x=\"457\" y=\"67\">\n    <value name=\"VALUE\">\n      <shadow type=\"text\" id=\"3cp9hM5kN2]Xb{zmKCB@\">\n        <field name=\"TEXT\">/start</field>\n      </shadow>\n    </value>\n    <next>\n      <block type=\"chat_say\" id=\"l;?R,0E%ti`V/9z{aHe,\">\n        <value name=\"VALUE\">\n          <shadow type=\"text\" id=\"|E,3eu~1Rl-E:?J3XNWG\">\n            <field name=\"TEXT\">Hello!</field>\n          </shadow>\n        </value>\n        <next>\n          <block type=\"data_setvariableto\" id=\"39bxL2I$88|Pqf!v~n[t\">\n            <field name=\"VARIABLE\" id=\"H42ZX:dR5{Bwui#r]te+\" variabletype=\"\">count</field>\n            <value name=\"VALUE\">\n              <shadow type=\"text\" id=\"d:z^AEt({Jban;?-[FT?\">\n                <field name=\"TEXT\">0</field>\n              </shadow>\n            </value>\n            <next>\n              <block type=\"control_repeat\" id=\"VfE`?a4`c4b?^D8rc0`y\">\n                <value name=\"TIMES\">\n                  <shadow type=\"math_whole_number\" id=\"(CK(@[qP{:*ieF,SchkU\">\n                    <field name=\"NUM\">3</field>\n                  </shadow>\n                </value>\n                <statement name=\"SUBSTACK\">\n                  <block type=\"control_wait\" id=\"!TKm=kc/kiDz1L|3!}x9\">\n                    <value name=\"DURATION\">\n                      <shadow type=\"math_positive_number\" id=\"c(3_}!Q!6NC.VYsG_uX1\">\n                        <field name=\"NUM\">1</field>\n                      </shadow>\n                    </value>\n                    <next>\n                      <block type=\"data_changevariableby\" id=\"i,/S]2-SY5HkVt5Y*VbA\">\n                        <field name=\"VARIABLE\" id=\"H42ZX:dR5{Bwui#r]te+\" variabletype=\"\">count</field>\n                        <value name=\"VALUE\">\n                          <shadow type=\"math_number\" id=\"k[{%pQ%?[DL}c{)chMCK\">\n                            <field name=\"NUM\">1</field>\n                          </shadow>\n                        </value>\n                        <next>\n                          <block type=\"chat_say\" id=\"^#ie|?!BUAKR6w6{9gmS\">\n                            <value name=\"VALUE\">\n                              <shadow type=\"text\" id=\"R^|DzbVW3FjdAVAb{Lu`\">\n                                <field name=\"TEXT\">Hello!</field>\n                              </shadow>\n                              <block type=\"operator_join\" id=\"FGPL0wp@pECda4C(?=3t\">\n                                <value name=\"STRING1\">\n                                  <shadow type=\"text\" id=\"bv0l/.Cr]Of`|S5]`|bn\">\n                                    <field name=\"TEXT\">Counted to </field>\n                                  </shadow>\n                                </value>\n                                <value name=\"STRING2\">\n                                  <shadow type=\"text\" id=\"20D{!]n+UE%W[#RTdH+t\">\n                                    <field name=\"TEXT\">world</field>\n                                  </shadow>\n                                  <block type=\"data_variable\" id=\"^jsCxw}+j98X!,JA=xTV\">\n                                    <field name=\"VARIABLE\" id=\"H42ZX:dR5{Bwui#r]te+\" variabletype=\"\">count</field>\n                                  </block>\n                                </value>\n                              </block>\n                            </value>\n                          </block>\n                        </next>\n                      </block>\n                    </next>\n                  </block>\n                </statement>\n                <next>\n                  <block type=\"chat_say\" id=\"]GTkQkA]]~KdJ?(G$kj2\">\n                    <value name=\"VALUE\">\n                      <shadow type=\"text\" id=\"c)S_2SAcnx]whz%%i^p3\">\n                        <field name=\"TEXT\">See?</field>\n                      </shadow>\n                    </value>\n                  </block>\n                </next>\n              </block>\n            </next>\n          </block>\n        </next>\n      </block>\n    </next>\n  </block>\n</xml>">>).

-define(SINGLE_LINE_PROGRAM,
        #user_program_entry{ id=?SINGLE_LINE_PROGRAM_ID
                           , user_id=?SINGLE_LINE_PROGRAM_USER_ID
                           , program_name=?SINGLE_LINE_PROGRAM_NAME
                           , program_type=?SINGLE_LINE_PROGRAM_TYPE
                           , program_parsed=#{ <<"blocks">> => [[ ?SINGLE_LINE_PROGRAM_TRIGGER
                                                                  | ?SINGLE_LINE_PROGRAM_INSTRUCTIONS ]]

                                             , <<"variables">> => ?SINGLE_LINE_PROGRAM_VARIABLES
                                             }
                           , program_orig=?SINGLE_LINE_PROGRAM_ORIG
                           }).


-define(SINGLE_LINE_PROGRAM_INITIALIZATION,
        #program_state{ program_id=?SINGLE_LINE_PROGRAM_ID
                      , variables=?SINGLE_LINE_PROGRAM_VARIABLES
                      , threads=[]
                      , triggers=[#program_trigger{ condition=?SINGLE_LINE_PROGRAM_TRIGGER
                                                  , subprogram=?SINGLE_LINE_PROGRAM_INSTRUCTIONS
                                                  }
                                 ]
                      }).
