%%%-------------------------------------------------------------------
%% @doc automate_service_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry_query).

%% API
-export([ is_enabled_for_user/2
        , get_how_to_enable/2
        , call/5
        , get_monitor_id/2
        , send_registration_data/3
        ]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").


%%====================================================================
%% API functions
%%====================================================================
is_enabled_for_user({Module, Params}, Username) ->
    Module:is_enabled_for_user(Username, Params);

is_enabled_for_user(Module, Username) ->
    Module:is_enabled_for_user(Username).

get_how_to_enable({Module, Params}, UserInfo) ->
    Module:get_how_to_enable(UserInfo, Params);

get_how_to_enable(Module, UserInfo) ->
    Module:get_how_to_enable(UserInfo).

-spec call(module() | {module(), any()}, binary(), any(), #program_thread{}, binary()) -> {ok, #program_thread{}, any()}.
call({Module, Params}, Action, Values, Thread, UserId) ->
    Module:call(Action, Values, Thread, UserId, Params);

call(Module, Action, Values, Thread, UserId) ->
    Module:call(Action, Values, Thread, UserId).

get_monitor_id({Module, Params}, UserId) ->
    Module:get_monitor_id(UserId, Params);

get_monitor_id(Module, UserId) ->
    Module:get_monitor_id(UserId).

send_registration_data({Module, Params}, UserId, RegistrationData) ->
    Module:send_registration_data(UserId, RegistrationData, Params);

send_registration_data(Module, UserId, RegistrationData) ->
    Module:send_registration_data(UserId, RegistrationData).
