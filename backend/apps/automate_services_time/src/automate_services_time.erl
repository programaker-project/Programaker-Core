%%%-------------------------------------------------------------------
%% @doc Timekeeping service main module.
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_time).

%% Service API
-export([ start_link/0
        , get_description/0
        , get_uuid/0
        , get_name/0
        , get_monitor_id/0
        , is_enabled_for_user/1
        , get_how_to_enable/1
        , listen_service/2
        , call/4
        ]).

-include("./definitions.hrl").
-include("../../automate_channel_engine/src/records.hrl").
-include("../../automate_testing/src/testing.hrl").
-define(SLEEP_RESOULUTION_MS, 500).

%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    spawn_timekeeper().

%% This one can be considered static.
get_uuid() ->
    ?TIME_SERVICE_UUID.

get_name() ->
    <<"Timekeeping">>.

get_description() ->
    <<"Timekeeping service.">>.

get_monitor_id() ->
    case automate_service_registry:get_config_for_service(get_uuid(), monitor_id) of
        {error, not_found} ->
            %% No monitor associated with this service
            {ok, ChannelId} = automate_channel_engine:create_channel(),
            automate_service_registry:set_config_for_service(get_uuid(), monitor_id, ChannelId),
            {ok, ChannelId};
        {ok, ChannelId} ->
            {ok, ChannelId}
    end.

-spec listen_service(owner_id(), {_, _}) -> ok.
listen_service(_Owner, _Selector) ->
    {ok, ChannelId} = get_monitor_id(),
    automate_channel_engine:listen_channel(ChannelId).

call(get_utc_hour, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {Hour, _Min, _Sec}} = calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp())),
    {ok, Thread, Hour};

call(get_utc_minute, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {_Hour, Min, _Sec}} = calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp())),
    {ok, Thread, Min};

call(get_utc_seconds, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {_Hour, _Min, Sec}} = calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp())),
    {ok, Thread, Sec};

call(get_tz_hour, [Timezone], Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {Hour, _Min, _Sec}} = qdate:to_date(Timezone, prefer_standard, calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp()))),
    {ok, Thread, Hour};

call(get_tz_minute, [Timezone], Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {_Hour, Min, _Sec}} = qdate:to_date(Timezone, prefer_standard, calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp()))),
    {ok, Thread, Min};

call(get_tz_seconds, [Timezone], Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {_Hour, _Min, Sec}} = qdate:to_date(Timezone, prefer_standard, calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp()))),
    {ok, Thread, Sec};

call(get_tz_day_of_month, [Timezone], Thread, _UserId) ->
    {{_Y1970, _Mon, Day}, {_Hour, _Min, _Sec}} = qdate:to_date(Timezone, prefer_standard, calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp()))),
    {ok, Thread, Day};

call(get_tz_month_of_year, [Timezone], Thread, _UserId) ->
    {{_Y1970, Mon, _Day}, {_Hour, _Min, _Sec}} = qdate:to_date(Timezone, prefer_standard, calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp()))),
    {ok, Thread, Mon};

call(get_tz_year, [Timezone], Thread, _UserId) ->
    {{Y1970, _Mon, _Day}, {_Hour, _Min, _Sec}} = qdate:to_date(Timezone, prefer_standard, calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp()))),
    {ok, Thread, Y1970};

call(get_tz_day_of_week, [_Timezone], Thread, _UserId) ->
    {{Y1970, Mon, Day}, {_Hour, _Min, _Sec}} = calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp())),
    %% Note that technically, calendar:day_of_the_week takes a Year, not Year1970 .
    %%  It should not affect this calculation, but keep it in mind.
    %%  See http://erlang.org/doc/man/calendar.html#type-year
    DayOfWeek = calendar:day_of_the_week(Y1970, Mon, Day),
    {ok, Thread, DayOfWeek};

call(<<"utc_is_day_of_week">>, [DayOfWeek], Thread, _UserId) ->
    {{Y1970, Mon, Day}, {_Hour, _Min, _Sec}} = calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp())),
    %% Note that technically, calendar:day_of_the_week takes a Year, not Year1970 .
    %%  It should not affect this calculation, but keep it in mind.
    %%  See http://erlang.org/doc/man/calendar.html#type-year
    Id = day_of_week_to_id(calendar:day_of_the_week(Y1970, Mon, Day)),
    {ok, Thread, Id == DayOfWeek}.

day_of_week_to_id(1) -> <<"mon">>;
day_of_week_to_id(2) -> <<"tue">>;
day_of_week_to_id(3) -> <<"wed">>;
day_of_week_to_id(4) -> <<"thu">>;
day_of_week_to_id(5) -> <<"fri">>;
day_of_week_to_id(6) -> <<"sat">>;
day_of_week_to_id(7) -> <<"sun">>.

%% Is enabled for all users
is_enabled_for_user(_Owner) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(_) ->
    {error, not_found}.


%%====================================================================
%% Timekeeping service
%%====================================================================
spawn_timekeeper() ->
    io:fwrite("[~p] Spawining Timekeeper~n", [node()]),
    case automate_coordination:run_task_not_parallel(
           fun() ->
                   io:fwrite("[~p] Timekeeper started~n", [self()]),
                   {ok, ChannelId} = get_monitor_id(),
                   {ok, _} = automate_service_registry:register_public(automate_services_time),
                   timekeeping_loop(ChannelId, {{0, 0, 0}, {0, 0, 0}})
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


timekeeping_loop(ChannelId, {{LYear, LMonth, LDay}, {LHour, LMin, LSec}}) ->
    DateTime = calendar:now_to_datetime(?CORRECT_EXECUTION_TIME(erlang:timestamp())),
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    case (Sec =/= LSec) orelse (Min =/= LMin) orelse (Hour =/= LHour) of
        true ->
            StrTime = binary:list_to_bin(lists:flatten(io_lib:format("~p:~p:~p", [Hour, Min, Sec]))),
            GregorianSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
            automate_channel_engine:send_to_channel(ChannelId,
                                                    #{ ?CHANNEL_MESSAGE_CONTENT => StrTime
                                                     , <<"full">> => #{ <<"year">> => Year
                                                                      , <<"month">> => Month
                                                                      , <<"day">> => Day

                                                                      , <<"hour">> => Hour
                                                                      , <<"minute">> => Min
                                                                      , <<"second">> => Sec
                                                                      , <<"__as_gregorian_seconds">> => GregorianSeconds
                                                                      }
                                                     , <<"as_list">> => [ Hour, Min, Sec]
                                                     , <<"key">> => <<"utc_time">>
                                                     , <<"service_id">> => get_uuid()
                                                     });
        false ->
            ok
    end,
    case (Year =/= LYear) orelse (Month =/= LMonth) orelse (Day =/= LDay) of
        true ->
            StrDate = binary:list_to_bin(lists:flatten(io_lib:format("~p/~p/~p", [Year, Month, Day]))),
            automate_channel_engine:send_to_channel(ChannelId,
                                                    #{ ?CHANNEL_MESSAGE_CONTENT => StrDate
                                                     , <<"full">> => #{ <<"year">> => Year
                                                                      , <<"month">> => Month
                                                                      , <<"day">> => Day

                                                                      , <<"hour">> => Hour
                                                                      , <<"minute">> => Min
                                                                      , <<"second">> => Sec
                                                                      }
                                                     , <<"as_list">> => [ Year, Month, Day, calendar:day_of_the_week(Year, Month, Day)]
                                                     , <<"key">> => <<"utc_date">>
                                                     , <<"service_id">> => get_uuid()
                                                     });
        false ->
            ok
    end,
    timer:sleep(?SLEEP_RESOULUTION_MS), % Wait for less than a second
    timekeeping_loop(ChannelId, {{Year, Month, Day}, {Hour, Min, Sec}}).
