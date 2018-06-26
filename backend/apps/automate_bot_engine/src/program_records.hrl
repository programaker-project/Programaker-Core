-record(program_trigger, { condition  :: map()
                         , subprogram :: [any()]
                         }).

-record(program_thread, { position :: [integer()]
                        , program  :: [any()]
                        }).

-record(program_state, { variables   :: [any()]
                       , threads :: [#program_thread{}]
                       , triggers    :: [#program_trigger{}]
                       }).
