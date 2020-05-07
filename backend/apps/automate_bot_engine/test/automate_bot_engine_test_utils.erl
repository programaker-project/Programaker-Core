-module(automate_bot_engine_test_utils).

-export([ build_ast/1
        , block_val/1
        ]).

-include("../src/instructions.hrl").

%%====================================================================
%% API
%%====================================================================

build_ast(Instructions) ->
    lists:map(fun(I) -> build_ast_instruction(I) end, Instructions).

block_val(Instruction) ->
    #{ ?TYPE => ?VARIABLE_BLOCK
     , ?VALUE => [ build_ast_instruction(Instruction)
                 ]
     }.

build_ast_instruction(Contents) when is_list(Contents) ->
    #{ ?CONTENTS => lists:map(fun(I) -> build_ast_instruction(I) end, Contents)
     };
build_ast_instruction({Name}) ->
    #{ ?TYPE => Name
     };
build_ast_instruction({Name, Args}) ->
    #{ ?TYPE => Name
     , ?ARGUMENTS => Args
     };
build_ast_instruction({Name, Args, Contents}) ->
    #{ ?TYPE => Name
     , ?ARGUMENTS => Args
     , ?CONTENTS => lists:map(fun(I) -> build_ast_instruction(I) end, Contents)
     }.
