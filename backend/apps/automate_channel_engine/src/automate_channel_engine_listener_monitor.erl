%%%-------------------------------------------------------------------
%% @doc automate_channel_engine_listener_monitor public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_channel_engine_listener_monitor).

-export([ start_link/0
        , monitor_listener/1
        ]).

-include("records.hrl").
-define(BACKEND, automate_channel_engine_mnesia_backend).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    case automate_coordination:run_task_not_parallel(
           fun() ->
                   yes = global:re_register_name(?MODULE, self()),
                   process_flag(trap_exit, true),
                   loop()
           end, ?MODULE) of
        {started, Pid} ->
            link(Pid),
            {ok, Pid};
        {already_running, Pid} ->
            link(Pid),
            {ok, Pid};
        {error, Error} ->
            {error, Error}
    end.


-spec monitor_listener(pid()) -> ok.
monitor_listener(Pid) ->
    global:send(?MODULE, { monitor, self(), Pid }),
    receive { ?MODULE, Response } ->
            Response
    end.

%%====================================================================
%% Private API
%%====================================================================
loop() ->
    receive
        { monitor, Answer, Pid } ->
            %% A link() is used here instead of a monitor to avoid a memory
            %% leak-like behavior when the same Pid is monitored again and
            %% again.
            %%
            %% Actually here the monitoring is not bidirectional (the listener
            %% process doesn't care about what happens with this daemon) but as
            %% errors here should be fairly unusual and adding de-duplication
            %% logic to this process will make it more complex we will skip it for now.
            erlang:link(Pid),
            case Answer of
                _ when is_pid(Answer) ->
                    Answer ! { ?MODULE, ok };
                _ ->
                    ok
            end,
            loop();
        { 'EXIT', Pid, _Reason } ->
            ok = ?BACKEND:remove_listener(Pid),
            loop();
        stop ->
            ok;
        Msg ->
            automate_logging:log_platform(warning, io_lib:format("Unknown message on listener monitor: ~p", [Msg])),
            loop()
    end.
