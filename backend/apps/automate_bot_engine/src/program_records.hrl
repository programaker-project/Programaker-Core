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

%% Error types
-record(index_not_in_list, { list_name :: binary()
                           , index :: non_neg_integer()
                           , max :: non_neg_integer()
                           }).
-record(invalid_list_index_type, { list_name :: binary()
                                 , index :: any()
                                 }).

-record(list_not_set, { list_name :: binary()
                      }).

-record(variable_not_set, { variable_name :: binary()
                          }).

-type program_error_type() :: #index_not_in_list{} | #invalid_list_index_type{}
                            | #list_not_set{} | #variable_not_set{}.

-record(program_error, { error :: program_error_type()
                       , block_id :: binary() | undefined
                       }).
