-module(automate_bot_engine_test_utils).

-export([ build_ast/1
        , block_val/1
        , create_anonymous_program/0
        , create_user_program/1
        , wait_for_program_alive/3
        , wait_for_check_ok/3
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

create_anonymous_program() ->
    {Username, _UserId} = create_random_user(),

    ProgramName = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    {ok, ProgramId} = automate_storage:create_program(Username, ProgramName),
    {Username, ProgramName, ProgramId}.

create_user_program(UserId) ->
    ProgramName = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    {ok, ProgramId} = automate_storage:create_program(UserId, ProgramName),
    {ok, ProgramId}.

create_random_user() ->
    Username = binary:list_to_bin(uuid:to_string(uuid:uuid4())),
    Password = undefined,
    Email = binary:list_to_bin(uuid:to_string(uuid:uuid4())),

    {ok, UserId} = automate_storage:create_user(Username, Password, Email, ready),
    {Username, UserId}.

%%====================================================================
%% Util functions
%%====================================================================
wait_for_program_alive(_Pid, 0, _SleepTime) ->
    {error, timeout};

wait_for_program_alive(ProgramId, TestTimes, SleepTime) ->
    case automate_storage:get_program_pid(ProgramId) of
        {ok, _} ->
            ok;
        {error, not_running} ->
            timer:sleep(SleepTime),
            wait_for_program_alive(ProgramId, TestTimes - 1, SleepTime)
    end.


wait_for_check_ok(_Check, 0, _SleepTime) ->
    {error, timeout};
wait_for_check_ok(Check, TestTimes, SleepTime) ->
    case Check() of
        true -> ok;
        false ->
            timer:sleep(SleepTime),
            wait_for_check_ok(Check, TestTimes - 1, SleepTime)
    end.
