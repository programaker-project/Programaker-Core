-record(program_trigger, { condition  :: map()
                         , subprogram :: [any()]
                         }).

-record(program_thread, { position :: [integer()]  % Instruction index on the thread
                        , program  :: [any()]      % Program run by the thread
                        , global_memory :: map()   % Thread-specific values TODO: rename
                        , instruction_memory :: map() % Memory held for each individual instruction on the program
                        , program_id :: binary()      % ID of the program being run
                        }).

-record(program_permissions, { owner_user_id :: binary()
                             }).

-record(program_state, { program_id   :: binary()
                       , permissions  :: #program_permissions{} | undefined
                       , variables    :: [any()]
                       , triggers     :: [#program_trigger{}]
                       , enabled=true :: boolean()
                       }).
