%%%-------------------------------------------------------------------
%% @doc automate_stats public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_stats).

%% Application callbacks
-export([ add_metric/4
        , set_metric/4
        , log_observation/3
        , format/1
        , remove_metric/2
        ]).

%% Internal calls
-export([ prepare/0
        ]).

-type metric_type() :: boolean | gauge | counter.

%%====================================================================
%% Stat logging API
%%====================================================================
-spec add_metric(metric_type(), atom() | binary(), binary(), [atom() | binary()]) -> ok.
add_metric(Type, Name, Description, Labels) ->
    Module = get_module_for_type(Type),
    Module:declare([{name, Name}, {labels, Labels}, {help, Description}]),
    ok.

-spec set_metric(metric_type(), atom() | binary(), any(), [atom() | binary()]) -> ok.
set_metric(Type, Name, Value, Labels) ->
    Module = get_module_for_type(Type),
    Module:set(Name, Labels, Value),
    ok.


-spec log_observation(counter, atom() | binary(), [atom() | binary()]) -> ok.
log_observation(counter, Name, Labels) ->
    prometheus_counter:inc(Name, Labels),
    ok.

-spec remove_metric(metric_type(), atom() | binary()) -> ok.
remove_metric(Type, Name) ->
    Module = get_module_for_type(Type),
    try Module:remove(Name) of
        _ -> ok
    catch _:_ ->
            ok
    end.

%%====================================================================
%% Stat retrieval API
%%====================================================================
format(prometheus) ->
    update_internal_metrics(), %% TODO: Avoid too much calling here
    prometheus_text_format:format().

update_internal_metrics() ->
    %% Services
    Services = [ automate_storage_sup

               , automate_channel_engine
               , automate_channel_engine_sup

               , automate_rest_api_sup

               , automate_service_registry_sup

               , automate_bot_engine_runner_sup
               , automate_bot_engine_thread_runner_sup
               , automate_bot_engine_sup

               , automate_monitor_engine_runner_sup
               , automate_monitor_engine_sup

               , automate_service_port_engine_sup
               ],

    lists:foreach(fun (S) ->
                          set_metric(boolean, automate_service,
                                     whereis(S) =/= undefined, [S])
                  end, Services),

    %% Bots
    try
        supervisor:count_children(automate_bot_engine_runner_sup)
    of Bots ->
            set_metric(gauge, automate_bot_count,
                       proplists:get_value(workers, Bots), [total]),

            set_metric(gauge, automate_bot_count,
                       proplists:get_value(active, Bots), [running])
    catch BotErrNS:BotErr:BotStackTrace ->
            io:fwrite("Error counting bots: ~p~n", [{BotErrNS, BotErr, BotStackTrace}]),
            set_metric(gauge, automate_bot_count, 0, [running])
    end,

    %% Threads
    try
        supervisor:count_children(automate_bot_engine_thread_runner_sup)
    of Threads ->
            set_metric(gauge, automate_program_thread_count,
                       proplists:get_value(workers, Threads), [total]),

            set_metric(gauge, automate_program_thread_count,
                       proplists:get_value(active, Threads), [running])
    catch ThreadErrNS:ThreadErr:ThreadStackTrace ->
            io:fwrite("Error counting threads: ~p~n", [{ThreadErrNS, ThreadErr, ThreadStackTrace}]),
            set_metric(gauge, automate_program_thread_count, 0, [running])
    end,

    %% Monitors
    try
        supervisor:count_children(automate_monitor_engine_runner_sup)
    of Monitors ->
            set_metric(gauge, automate_monitor_count,
                       proplists:get_value(workers, Monitors), [total]),

            set_metric(gauge, automate_monitor_count,
                       proplists:get_value(active, Monitors), [running])
    catch MonitorErrNS:MonitorErr:MonitorStackTrace ->
            io:fwrite("Error counting monitors: ~p~n", [{MonitorErrNS, MonitorErr, MonitorStackTrace}]),
            set_metric(gauge, automate_monitor_count, 0, [running])
    end,

    %% Services
    case automate_service_registry:get_all_public_services() of
        {ok, PublicServices} ->
            set_metric(gauge, automate_service_count,
                       maps:size(PublicServices), [public]),

            set_metric(gauge, automate_service_count,
                       automate_service_registry:count_all_services(), [all]);
        {error, _, _} ->
            remove_metric(gauge, automate_service_count)
    end,

    %% Users
    { ok
    , UserCount, DailyRegisteredUsers, WeeklyRegisteredUsers, MonthlyRegisteredUsers
    , LoggedUsersLastHour, LoggedUsersLastDay, LoggedUsersLastWeek, LoggedUsersLastMonth
    } = automate_storage_stats:get_user_metrics(),
    set_metric(gauge, automate_user_count, UserCount, [registered]),
    set_metric(gauge, automate_daily_registered_user, DailyRegisteredUsers, [registered]),
    set_metric(gauge, automate_weekly_registered_user, WeeklyRegisteredUsers, [registered]),
    set_metric(gauge, automate_monthly_registered_user, MonthlyRegisteredUsers, [registered]),

    set_metric(gauge, automate_logged_users_last_hour, LoggedUsersLastHour, [registered]),
    set_metric(gauge, automate_logged_users_last_day, LoggedUsersLastDay, [registered]),
    set_metric(gauge, automate_logged_users_last_week, LoggedUsersLastWeek, [registered]),
    set_metric(gauge, automate_logged_users_last_month, LoggedUsersLastMonth, [registered]),
    ok.


%%====================================================================
%% Functions for internal usage
%%====================================================================
prepare() ->
    add_metric(boolean, automate_service, <<"State of automate service.">>, [name]),

    add_metric(gauge, automate_bot_count, <<"Automate's bot.">>, [state]),
    add_metric(gauge, automate_program_thread_count, <<"Automate's program thread count.">>, [state]),
    add_metric(gauge, automate_monitor_count, <<"Automate's monitor.">>, [state]),
    add_metric(gauge, automate_service_count, <<"Automate's services.">>, [visibility]),

    add_metric(gauge, automate_user_count, <<"Automate's user.">>, [state]),
    add_metric(gauge, automate_daily_registered_user, <<"Users registered in the last 24 hours.">>, [state]),
    add_metric(gauge, automate_weekly_registered_user, <<"Users registered in the last 7 days.">>, [state]),
    add_metric(gauge, automate_monthly_registered_user, <<"Users registered in the last 28 days.">>, [state]),

    add_metric(gauge, automate_logged_users_last_hour, <<"Users logged in the last hour.">>, [state]),
    add_metric(gauge, automate_logged_users_last_day, <<"Users logged in the last 24 hours.">>, [state]),
    add_metric(gauge, automate_logged_users_last_week, <<"Users logged in the last 7 days.">>, [state]),
    add_metric(gauge, automate_logged_users_last_month, <<"Users logged in the last 28 days.">>, [state]),
    ok.


get_module_for_type(boolean) ->
    prometheus_boolean;
get_module_for_type(gauge) ->
    prometheus_gauge;
get_module_for_type(counter) ->
    prometheus_counter.
