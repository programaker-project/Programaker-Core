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
        , get_internal_metrics/0
        ]).

%% Internal calls
-export([ prepare/0
        ]).
-include("./records.hrl").

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

-spec get_internal_metrics() -> {ok, #internal_metrics{}, [iolist()]}.
get_internal_metrics() ->
    Services = [ automate_storage_sup

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

    ServiceCounts = maps:from_list(lists:map(fun (S) ->
                                                     {S, whereis(S) =/= undefined}
                                             end, Services)),

    Errors = [],

    %% Bots
    {BotCount, Err1} = try supervisor:count_children(automate_bot_engine_runner_sup)
               of Bots ->
                               { maps:from_list(lists:filter(fun({K, _}) -> (K =:= active) or (K =:= workers) end, Bots))
                               , Errors}
               catch BotErrNS:BotErr:BotStackTrace ->
                       { #{ active => undefined, worker => undefined }
                       , [{bot_engine_programs, {BotErrNS, BotErr, BotStackTrace}} | Errors]}
               end,

    %% Threads
    { ThreadCount, Err2 } = try supervisor:count_children(automate_bot_engine_thread_runner_sup)
                               of Threads ->
                                    { maps:from_list(lists:filter(fun({K, _}) -> (K =:= active) or (K =:= workers) end, Threads))
                                    , Err1}
                               catch ThreadErrNS:ThreadErr:ThreadStackTrace ->
                                       { #{ active => undefined, worker => undefined }
                                       , [{bot_engine_threads, {ThreadErrNS, ThreadErr, ThreadStackTrace}} | Err1]}
                               end,

    %% Monitors
    { MonitorCount, Err3 } = try supervisor:count_children(automate_monitor_engine_runner_sup)
                             of Monitors ->
                                     { maps:from_list(lists:filter(fun({K, _}) -> (K =:= active) or (K =:= workers) end, Monitors))
                                     , Err2}
                             catch MonitorErrNS:MonitorErr:MonitorStackTrace ->
                                     { #{ active => undefined, worker => undefined }
                                     , [{monitor_engine, {MonitorErrNS, MonitorErr, MonitorStackTrace}} | Err2]}
                             end,

    %% Services
    { ServiceCount, Err4 } = case automate_service_registry:get_all_public_services() of
                                 {ok, PublicServices} ->
                                     { #{ public => maps:size(PublicServices)
                                        , all => automate_service_registry:count_all_services()
                                        }
                                     , Err3 };
                                 {error, Reason, _} ->
                                     { #{ all => undefined, public => undefined }
                                     , [ { public_services, Reason } | Err3 ]}
                             end,

    %% Users
    { ok
    , UserCount, RegisteredUsersLastDay, RegisteredUsersLastWeek, RegisteredUsersLastMonth
    , LoggedUsersLastHour, LoggedUsersLastDay, LoggedUsersLastWeek, LoggedUsersLastMonth
    } = automate_storage_stats:get_user_metrics(),

    { ok
    , NumBridgesPublic, NumBridgesPrivate
    , NumConnections, NumUniqueConnections
    , NumMessagesOnFlight
    } = automate_service_port_engine_stats:get_bridge_metrics(),

    {ok
    , #internal_metrics{ services_active=ServiceCounts
                       , bot_count=BotCount
                       , thread_count=ThreadCount
                       , monitor_count=MonitorCount
                       , service_count=ServiceCount
                       , user_stats=#user_stat_metrics{ count=UserCount
                                                      , registered_last_day=RegisteredUsersLastDay
                                                      , registered_last_week=RegisteredUsersLastWeek
                                                      , registered_last_month=RegisteredUsersLastMonth
                                                      , logged_last_hour=LoggedUsersLastHour
                                                      , logged_last_day=LoggedUsersLastDay
                                                      , logged_last_week=LoggedUsersLastWeek
                                                      , logged_last_month=LoggedUsersLastMonth
                                                      }
                       , bridge_stats=#bridge_stat_metrics{ public_count=NumBridgesPublic
                                                          , private_count=NumBridgesPrivate
                                                          , connections=NumConnections
                                                          , unique_connections=NumUniqueConnections
                                                          , messages_on_flight=NumMessagesOnFlight
                                                          }
                       }
    , Err4}.

%%====================================================================
%% Functions for internal usage
%%====================================================================
update_internal_metrics() ->
    %% Services
    {ok, #internal_metrics{ services_active=Services
                          , bot_count=BotCount
                          , thread_count=ThreadCount
                          , monitor_count=MonitorCount
                          , service_count=ServiceCount
                          , user_stats=UserStats
                          , bridge_stats=BridgeStats
           }, Errors} = get_internal_metrics(),
    maps:map(fun(Service, Active) ->
                     set_metric(boolean, automate_service, Active, [Service])
             end, Services),

    maps:map(fun(Category, Count) ->
                      set_metric(gauge, automate_bot_count, Count, [Category])
              end, BotCount),

    maps:map(fun(Category, Count) ->
                      set_metric(gauge, automate_program_thread_count, Count, [Category])
              end, ThreadCount),

    maps:map(fun(Category, Count) ->
                      set_metric(gauge, automate_monitor_count, Count, [Category])
              end, MonitorCount),

    maps:map(fun(Category, Count)  ->
                      set_metric(gauge, automate_service_count, Count, [Category])
              end, ServiceCount),

    %% Users
    #user_stat_metrics{ count=UserCount
                      , registered_last_day=RegisteredUsersLastDay
                      , registered_last_week=RegisteredUsersLastWeek
                      , registered_last_month=RegisteredUsersLastMonth
                      , logged_last_hour=LoggedUsersLastHour
                      , logged_last_day=LoggedUsersLastDay
                      , logged_last_week=LoggedUsersLastWeek
                      , logged_last_month=LoggedUsersLastMonth
                      } = UserStats,
    set_metric(gauge, automate_user_count, UserCount, [registered]),
    set_metric(gauge, automate_registered_users_last_day, RegisteredUsersLastDay, [registered]),
    set_metric(gauge, automate_registered_users_last_week, RegisteredUsersLastWeek, [registered]),
    set_metric(gauge, automate_registered_users_last_month, RegisteredUsersLastMonth, [registered]),

    set_metric(gauge, automate_logged_users_last_hour, LoggedUsersLastHour, [registered]),
    set_metric(gauge, automate_logged_users_last_day, LoggedUsersLastDay, [registered]),
    set_metric(gauge, automate_logged_users_last_week, LoggedUsersLastWeek, [registered]),
    set_metric(gauge, automate_logged_users_last_month, LoggedUsersLastMonth, [registered]),

    %% Bridges
    #bridge_stat_metrics{ public_count=NumBridgesPublic
                        , private_count=NumBridgesPrivate
                        , connections=NumConnections
                        , unique_connections=NumUniqueConnections
                        , messages_on_flight=NumMessagesOnFlight
                        } = BridgeStats,
    set_metric(gauge, automate_bridges_count, NumBridgesPublic, [public]),
    set_metric(gauge, automate_bridges_count, NumBridgesPrivate, [private]),
    set_metric(gauge, automate_bridges_connections_count, NumConnections, []),
    set_metric(gauge, automate_bridges_unique_connections_count, NumUniqueConnections, []),
    set_metric(gauge, automate_bridges_messages_on_flight_count, NumMessagesOnFlight, []),

    %% Program logs
    {ok, LogCountPerProgram} = automate_storage_stats:get_program_metrics(),
    ok = set_log_count_metrics(LogCountPerProgram),

    lists:map(fun({Module, Reason}) ->
                      case Reason of
                          {ErrorNS, Error, StackTrace} ->
                              automate_logging:log_platform(warning, ErrorNS, Error, StackTrace);
                          _ ->
                              automate_logging:log_platform(warning, io_lib:format("Error getting stats for ~p. Reason: ~p",
                                                                                   [Module, Reason]))
                      end
              end, Errors),

    ok.

set_log_count_metrics(LogCountPerProgram) ->
    %% No foreach, so we use maps:map/2
    maps:map(fun(ProgramId, Value) ->
                     maps:map(
                       fun(Severity, SubCategories) ->
                               maps:map(
                                 fun(Category, Count) ->
                                         set_metric(gauge, automate_program_log_count, Count, [ProgramId, Severity, Category])
                                 end, SubCategories)
                       end, Value),
                     ok
             end, LogCountPerProgram),
    ok.


prepare() ->
    add_metric(boolean, automate_service, <<"State of automate service.">>, [name]),

    add_metric(gauge, automate_bot_count, <<"Automate's bot.">>, [state]),
    add_metric(gauge, automate_program_thread_count, <<"Automate's program thread count.">>, [state]),
    add_metric(gauge, automate_monitor_count, <<"Automate's monitor.">>, [state]),
    add_metric(gauge, automate_service_count, <<"Automate's services.">>, [visibility]),
    add_metric(gauge, automate_program_log_count, <<"Logs generated by a program.">>, [program, severity, log_type]),

    add_metric(gauge, automate_bridges_count, <<"Number of bridges existing on the platform.">>, [visibility]),
    add_metric(gauge, automate_bridges_connections_count, <<"Number of bridge connections established to the platform.">>, []),
    add_metric(gauge, automate_bridges_unique_connections_count, <<"Number of bridges which have at least one established connection to the platform.">>, []),
    add_metric(gauge, automate_bridges_messages_on_flight_count, <<"Number of messages on flight to bridges.">>, []),

    add_metric(gauge, automate_user_count, <<"Automate's user.">>, [state]),
    add_metric(gauge, automate_registered_users_last_day, <<"Users registered in the last 24 hours.">>, [state]),
    add_metric(gauge, automate_registered_users_last_week, <<"Users registered in the last 7 days.">>, [state]),
    add_metric(gauge, automate_registered_users_last_month, <<"Users registered in the last 28 days.">>, [state]),

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
