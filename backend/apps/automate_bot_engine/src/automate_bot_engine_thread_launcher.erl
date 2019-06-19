%%%-------------------------------------------------------------------
%% @doc automate_bot_engine main runner, allows to span appropiately
%%      the bots.
%% @end
%%%-------------------------------------------------------------------

-module(automate_bot_engine_thread_launcher).

%% API
-export([ launch_thread/2
        ]).

-include("program_records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec launch_thread(binary(), #program_thread{}) -> {ok, binary()}.
launch_thread(ParentProgramId, Thread) ->
    automate_stats:log_observation(counter, automate_bot_thread_launch, [ParentProgramId]),

    {ok, ThreadId} = automate_storage:create_thread(ParentProgramId, Thread),
    ok = automate_bot_engine_thread_runner_sup:start(ThreadId),
    {ok, ThreadId}.
