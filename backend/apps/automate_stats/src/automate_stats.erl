%%%-------------------------------------------------------------------
%% @doc automate_stats public API
%% @end
%%%-------------------------------------------------------------------

-module(automate_stats).

-behaviour(application).

%% Application callbacks
-export([ add_metric/4
        , set_metric/4
        , log_observation/2
        , format/1
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


-spec log_observation(counter, atom() | binary()) -> ok.
log_observation(counter, Name) ->
    prometheus_counter:inc(Name),
    ok.

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
               , automate_bot_engine_sup

               , automate_monitor_engine_runner_sup
               , automate_monitor_engine_sup

               , automate_services_telegram_demux
               , automate_services_telegram_sup
               ],

    lists:foreach(fun (S) ->
                          set_metric(boolean, automate_service,
                                     whereis(S) =/= undefined, [S])
                  end, Services),

    %% Bots
    Bots = supervisor:count_children(automate_bot_engine_runner_sup),
    set_metric(gauge, automate_bot_count,
               proplists:get_value(workers, Bots), [total]),

    set_metric(gauge, automate_bot_count,
               proplists:get_value(active, Bots), [running]),

    %% Monitors
    Monitors = supervisor:count_children(automate_monitor_engine_runner_sup),
    set_metric(gauge, automate_monitor_count,
               proplists:get_value(workers, Monitors), [total]),

    set_metric(gauge, automate_monitor_count,
               proplists:get_value(active, Monitors), [running]),

    %% Services
    {ok, PublicServices} = automate_service_registry:get_all_public_services(),
    set_metric(gauge, automate_service_count,
               maps:size(PublicServices), [public]),

    set_metric(gauge, automate_service_count,
               automate_service_registry:count_all_services(), [all]),

    %% Users
    set_metric(gauge, automate_user_count,
               automate_storage_stats:count_users(), [registered]),

    ok.


%%====================================================================
%% Functions for internal usage
%%====================================================================
prepare() ->
    prometheus_boolean:declare([{name, automate_service}, {labels, [name]}, {help, "State of automate service."}]),

    prometheus_gauge:declare([{name, automate_bot_count}, {labels, [state]}, {help, "Automate's bot."}]),
    prometheus_gauge:declare([{name, automate_monitor_count}, {labels, [state]}, {help, "Automate's monitor."}]),
    prometheus_gauge:declare([{name, automate_service_count}, {labels, [visibility]}, {help, "Automate's services."}]),

    prometheus_gauge:declare([{name, automate_user_count}, {labels, [state]}, {help, "Automate's user."}]),
    ok.


get_module_for_type(boolean) ->
    prometheus_boolean;
get_module_for_type(gauge) ->
    prometheus_gauge;
get_module_for_type(counter) ->
    prometheus_counter.
