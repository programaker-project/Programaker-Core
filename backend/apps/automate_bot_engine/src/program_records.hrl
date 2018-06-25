-record(subprogram_state, { subid
                            %% Note that erlang 1-indexes lists!
                            %% For this reason, position also starts on 1.
                          , position
                          , ast
                          }).

-record(program_trigger, { condition  :: map()
                         , subprogram :: [any()]
                         }).

-record(program_state, { variables   :: [any()]
                       , subprograms :: [any()]
                       , triggers    :: #program_trigger{}
                       }).
