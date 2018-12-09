%%% @doc
%%% Test service to perform service registry tests.
%%% @end

-module(automate_service_registry_test_service).

-include("../src/records.hrl").

-export([ start_link/0
        , get_description/0
        , is_available_to_user/1
        , get_actions/0
        , get_uuid/0
        , get_name/0
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

is_available_to_user(_User) ->
    %% Available to anyone
    true.

get_actions() ->
    [
    ].

