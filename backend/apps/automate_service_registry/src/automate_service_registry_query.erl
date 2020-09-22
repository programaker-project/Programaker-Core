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
        , send_registration_data/4
        ]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").


%%====================================================================
%% API functions
%%====================================================================
-spec is_enabled_for_user(module() | {module(), any()}, owner_id()) -> {ok, boolean()}.
is_enabled_for_user({Module, Params}, Owner) ->
    Module:is_enabled_for_user(Owner, Params);

is_enabled_for_user(Module, Username) ->
    Module:is_enabled_for_user(Username).

get_how_to_enable({Module, Params}, UserInfo) ->
    Module:get_how_to_enable(UserInfo, Params);

get_how_to_enable(Module, UserInfo) ->
    Module:get_how_to_enable(UserInfo).

-spec call(module() | {module(), any()}, binary(), any(), #program_thread{}, owner_id()) -> {ok, #program_thread{}, any()}.
call({Module, Params}, Action, Values, Thread, Owner) ->
    Module:call(Action, Values, Thread, Owner, Params);

call(Module, Action, Values, Thread, Owner) ->
    Module:call(Action, Values, Thread, Owner).

-spec get_monitor_id(module() | {module(), any()}, owner_id()) -> {ok, binary()}.
get_monitor_id({Module, Params}, Owner) ->
    Module:get_monitor_id(Owner, Params);

get_monitor_id(Module, UserId) ->
    Module:get_monitor_id(UserId).

-spec send_registration_data(module() | {module(), any()}, owner_id(), any(), any()) -> {ok, any()}.
send_registration_data({Module, Params}, UserId, RegistrationData, RegistrationProperties) ->
    Module:send_registration_data(UserId, RegistrationData, Params, RegistrationProperties);

send_registration_data(Module, UserId, RegistrationData, RegistrationProperties) ->
    Module:send_registration_data(UserId, RegistrationData, [], RegistrationProperties).
