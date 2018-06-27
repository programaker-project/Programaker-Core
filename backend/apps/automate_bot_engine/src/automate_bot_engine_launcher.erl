%%%-------------------------------------------------------------------
%% @doc automate_bot_engine main runner, allows to span appropiately
%%      the bots.
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_launcher).

%% API
-export([ update_program/1
        , user_sent_telegram_message/4
        ]).

%%====================================================================
%% API functions
%%====================================================================
update_program(ProgramId) ->
    case get_program_pid(ProgramId) of
        { ok, PID } ->
            case process_info(PID) of
                undefined -> %% Not alive
                    automate_bot_engine_runner_sup:start(ProgramId);
                _ ->
                    automate_bot_engine_runner:update(PID)
            end;
        {error, not_running} ->
            automate_bot_engine_runner_sup:start(ProgramId)
    end.

user_sent_telegram_message(InternalUserId, ChatId, Content, BotName) ->
    case automate_storage:list_programs_from_userid(InternalUserId) of
        {ok, Programs} ->
            lists:map(fun({ProgramId, _ProgramName}) ->
                              {ok, Pid} = automate_storage:get_program_pid(ProgramId),
                              automate_bot_engine_runner:user_sent_message(Pid, ChatId, Content, BotName)
                      end, Programs)
    end.
%%====================================================================
%% Internal functions
%%====================================================================
get_program_pid(ProgramId) ->
    automate_storage:get_program_pid(ProgramId).
