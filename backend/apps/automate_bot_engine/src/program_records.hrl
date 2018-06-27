-record(program_trigger, { condition  :: map()
                         , subprogram :: [any()]
                         }).

-record(program_thread, { position :: [integer()]
                        , program  :: [any()]
                        , global_memory :: map()
                        }).

-record(program_state, { program_id  :: binary()
                       , variables   :: [any()]
                       , threads     :: [#program_thread{}]
                       , triggers    :: [#program_trigger{}]
                       }).
