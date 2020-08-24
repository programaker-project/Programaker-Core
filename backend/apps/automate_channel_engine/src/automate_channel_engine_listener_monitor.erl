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
                   yes = global:register_name(?MODULE, self()),
                   loop()
           end, ?MODULE) of
        {started, Pid} ->
            {ok, Pid};
        {already_running, Pid} ->
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
            erlang:monitor(process, Pid),
            Result = ok,
            case Answer of
                _ when is_pid(Answer) ->
                    Answer ! { ?MODULE, Result };
                _ ->
                    ok
            end,
            loop();
        { 'DOWN', _Ref, process, Pid, _Reason } ->
            ok = ?BACKEND:remove_listener(Pid),
            loop();
        stop ->
            ok;
        Msg ->
            automate_logging:log_platform(warning, io_lib:format("Unknown message on listener monitor: ~p", [Msg])),
            loop()
    end.
