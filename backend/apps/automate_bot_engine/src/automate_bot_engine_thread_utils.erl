-module(automate_bot_engine_thread_utils).

%% API
-export([ parse_program_thread/1
        , merge_thread_structures/2
        , build_check_next_action/1
        ]).

-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

parse_program_thread(#running_program_thread_entry{ position=Position
                                                  , instructions=Instructions
                                                  , memory=Memory
                                                  , instruction_memory=InstructionMemory
                                                  , parent_program_id=ParentProgramId
                                                  }) ->
    #program_thread{ position=Position
                   , program=Instructions
                   , global_memory=Memory
                   , instruction_memory=InstructionMemory
                   , program_id=ParentProgramId
                   }.

-spec merge_thread_structures(#running_program_thread_entry{}, #program_thread{}) -> #running_program_thread_entry{}.
merge_thread_structures(Thread, #program_thread{ position=Position
                                               , program=Instructions
                                               , global_memory=Memory
                                               , instruction_memory=InstructionMemory
                                               , program_id=ParentProgramId
                                               }) ->
    Thread#running_program_thread_entry{ position=Position
                                       , instructions=Instructions
                                       , memory=Memory
                                       , instruction_memory=InstructionMemory
                                       , parent_program_id=ParentProgramId
                                       }.

build_check_next_action(ExpectedMessages) ->
    fun(_, {Type, _Content}) ->
            case lists:member(Type, ExpectedMessages) of
                true ->
                    continue;
                _ ->
                    skip
            end
    end.
