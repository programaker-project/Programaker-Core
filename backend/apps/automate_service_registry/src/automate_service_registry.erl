%%%-------------------------------------------------------------------
%% @doc automate_service_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry).

%% API
-export([ get_all_public_services/0
        , get_all_services_for_user/1
        , register_public/1
        , register_private/1
        , allow_user/2
        ]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-define(BACKEND, automate_service_registry_mnesia_backend).

%%====================================================================
%% API functions
%%====================================================================
-spec get_all_public_services() -> {ok, [service_info_map()]} | {error, term(), string()}.
get_all_public_services() ->
    ?BACKEND:list_all_public().

-spec get_all_services_for_user(binary()) -> {ok, [service_info_map()]} | {error, term(), string()}.
get_all_services_for_user(UserId) ->
    ?BACKEND:get_all_services_for_user(UserId).

-spec register_public(module()) -> {ok, binary()}.
register_public(ServiceModule) ->
    Uuid = ServiceModule:get_uuid(),
    ok = ?BACKEND:register(Uuid,
                           true, %% Public
                           #{ name => ServiceModule:get_name()
                            , description => ServiceModule:get_description()
                            , module => ServiceModule
                            }),
    {ok, Uuid}.

-spec register_private(module()) -> {ok, binary()}.
register_private(ServiceModule) ->
    Uuid = ServiceModule:get_uuid(),
    ok = ?BACKEND:register(Uuid,
                           false, %% Private
                           #{ name => ServiceModule:get_name()
                            , description => ServiceModule:get_description()
                            , module => ServiceModule
                            }),
    {ok, Uuid}.

-spec allow_user(binary(), binary()) -> ok | {error, service_not_found}.
allow_user(ServiceId, UserId) ->
    ?BACKEND:allow_user(ServiceId, UserId).

%%====================================================================
%% Internal functions
%%====================================================================
