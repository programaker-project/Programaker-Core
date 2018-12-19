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
%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    ignore.

%% This will return a fine one: https://duckduckgo.com/?q=uuid&ia=answer
get_uuid() ->
    <<"0093325b-373f-4f1c-bace-4532cce79df4">>.

get_name() ->
    <<"Timekeeping">>.

get_description() ->
    <<"Timekeeping service.">>.

%% No monitor associated with this service
get_monitor_id(UserId) ->
    {error, not_found}.

call(get_utc_hour, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {Hour, _Min, _Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    {ok, Thread, Hour}.

%% Is enabled for all users
is_enabled_for_user(_Username) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(_) ->
    {error, not_found}.
