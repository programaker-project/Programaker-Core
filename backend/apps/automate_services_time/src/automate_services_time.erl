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
        , is_enabled_for_user/1
        , get_how_to_enable/1
        , get_monitor_id/1
        , call/4
        ]).

-include("../../automate_channel_engine/src/records.hrl").
-define(SLEEP_RESOULUTION_MS, 500).

%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    spawn_timekeeper().

%% This will return a fine one: https://duckduckgo.com/?q=uuid&ia=answer
get_uuid() ->
    <<"0093325b-373f-4f1c-bace-4532cce79df4">>.

get_name() ->
    <<"Timekeeping">>.

get_description() ->
    <<"Timekeeping service.">>.

%% No monitor associated with this service
get_monitor_id(_UserId) ->
    case automate_service_registry:get_config_for_service(get_uuid(), monitor_id) of
        {error, not_found} ->
            {ok, ChannelId} = automate_channel_engine:create_channel(),
            automate_service_registry:set_config_for_service(get_uuid(), monitor_id, ChannelId),
            {ok, ChannelId};
        {ok, ChannelId} ->
            {ok, ChannelId}
    end.

call(get_utc_hour, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {Hour, _Min, _Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    {ok, Thread, Hour};

call(get_utc_minute, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {_Hour, Min, _Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    {ok, Thread, Min};

call(get_utc_seconds, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {_Hour, _Min, Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    {ok, Thread, Sec}.

%% Is enabled for all users
is_enabled_for_user(_Username) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(_) ->
    {error, not_found}.


%%====================================================================
%% Timekeeping service
%%====================================================================
spawn_timekeeper() ->
    {ok, ChannelId} = get_monitor_id(none),
    {ok, spawn(fun () -> timekeeping_loop(ChannelId, {0, 0, 0}) end)}.


timekeeping_loop(ChannelId, {LHour, LMin, LSec}) ->
    io:fwrite("."),
    {_, {Hour, Min, Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    case (Sec =/= LSec) orelse (Min =/= LMin) orelse (Hour =/= LHour) of
        true ->
            %% automate_channel_engine:send_to_channel(ChannelId, {Hour, Min, Sec});
            StrTime = binary:list_to_bin(lists:flatten(io_lib:format("~p:~p:~p", [Hour, Min, Sec]))),
            automate_channel_engine:send_to_channel(ChannelId,
                                                    #{ ?CHANNEL_MESSAGE_CONTENT => StrTime
                                                     });
        false ->
            ok
    end,
    timer:sleep(?SLEEP_RESOULUTION_MS), % Wait for less than a second
    timekeeping_loop(ChannelId, {Hour, Min, Sec}).
