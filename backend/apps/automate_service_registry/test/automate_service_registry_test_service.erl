%%% @doc
%%% Test service to perform service registry tests.
%%% @end

-module(automate_service_registry_test_service).

-include("../src/records.hrl").

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
get_uuid() ->
    <<"b2173c01-465c-4f6e-99df-ba2bfc608e94">>.

get_name() ->
    <<"Test service">>.

get_monitor_id(_UserId) ->
    {error, not_found}.

start_link() ->
    ignore.

get_description() ->
    <<"Test module for service registry unit tests.">>.

is_enabled_for_user(_Username) ->
    %% Available to anyone
    true.

get_how_to_enable(_Userdata) ->
    none.

call(_Action, _Values, Thread, _UserId) ->
    %% No change
    {ok, Thread}.
