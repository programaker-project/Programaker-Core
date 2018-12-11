%%% @doc
%%% Test service to perform service registry tests.
%%% @end

-module(automate_service_registry_test_service).

-include("../src/records.hrl").

-export([ start_link/0
        , get_description/0
        , get_actions/0
        , get_uuid/0
        , get_name/0
        , is_enabled_for_user/1
        , get_how_to_enable/1
        ]).


%%====================================================================
%% Service API
%%====================================================================
get_uuid() ->
    <<"b2173c01-465c-4f6e-99df-ba2bfc608e94">>.

get_name() ->
    <<"Test service">>.

start_link() ->
    ignore.

get_description() ->
    <<"Test module for service registry unit tests.">>.

get_actions() ->
    [
    ].

is_enabled_for_user(_Username) ->
    %% Available to anyone
    true.

get_how_to_enable(_Userdata) ->
    none.
