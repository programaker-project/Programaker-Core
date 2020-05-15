%%%-------------------------------------------------------------------
%% @doc automate_service_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry).

%% API
-export([ get_all_public_services/0
        , get_all_services_for_user/1
        , get_service_by_id/2
        , register_public/1
        , register_private/1
        , update_service_module/3
        , allow_user/2

        , get_config_for_service/2
        , set_config_for_service/3

        , count_all_services/0
        , delete_service/2
        ]).

-define(SERVER, ?MODULE).
-include("records.hrl").
-define(BACKEND, automate_service_registry_mnesia_backend).
-define(MODULE_MAP, #{ name := binary()
                     , description := binary()
                     , module := {module(), [_]}
                     , uuid := binary()
                     }).

%%====================================================================
%% API functions
%%====================================================================
-spec get_all_public_services() -> {ok, service_info_map()} | {error, term(), string()}.
get_all_public_services() ->
    ?BACKEND:list_all_public().

-spec get_all_services_for_user(binary()) -> {ok, service_info_map()} | {error, term(), string()}.
get_all_services_for_user(UserId) ->
    ?BACKEND:get_all_services_for_user(UserId).

-spec get_service_by_id(binary(), binary()) -> {ok, service_entry()} | {error, not_found}.
get_service_by_id(ServiceId, UserId) ->
    ?BACKEND:get_service_by_id(ServiceId, UserId).

-spec register_public(module() | ?MODULE_MAP) -> {ok, binary()}.
register_public(ServiceModule) ->
    {Uuid, Data} = module_to_map(ServiceModule),
    ok = ?BACKEND:register(Uuid,
                           true, %% Public
                           Data),
    {ok, Uuid}.

-spec update_service_module(module() | ?MODULE_MAP,
                            binary(), binary()) -> ok.
update_service_module(Module, _ServiceId, _OwnerId) ->
    {Uuid, Data} = module_to_map(Module),
    ?BACKEND:update_service_module(Uuid, Data).

-spec register_private(module() | ?MODULE_MAP) -> {ok, binary()}.
register_private(ServiceModule) ->
    {Uuid, Data} = module_to_map(ServiceModule),
    ok = ?BACKEND:register(Uuid,
                           false, %% Private
                           Data),
    {ok, Uuid}.

-spec allow_user(binary(), binary()) -> ok | {error, service_not_found}.
allow_user(ServiceId, UserId) ->
    ?BACKEND:allow_user(ServiceId, UserId).

-spec get_config_for_service(binary(), atom()) -> {ok, any()} | {error, not_found}.
get_config_for_service(ServiceId, Property) ->
    ?BACKEND:get_config_for_service(ServiceId, Property).

-spec set_config_for_service(binary(), atom(), any()) -> ok | {error, atom()}.
set_config_for_service(ServiceId, Property, Value) ->
    ?BACKEND:set_config_for_service(ServiceId, Property, Value).

-spec count_all_services() -> number().
count_all_services() ->
    ?BACKEND:count_all_services().

-spec delete_service(binary(), binary()) -> ok.
delete_service(UserId, ServiceId) ->
    ?BACKEND:delete_service(UserId, ServiceId).



%%====================================================================
%% Internal functions
%%====================================================================
module_to_map(#{ name := Name
               , uuid := Uuid
               , description := Description
               , module := {Module, Params}
               }) ->
    { Uuid
    , #{ name => Name
       , description => Description
       , module => {Module, Params}
       }};

module_to_map(ServiceModule) ->
    { ServiceModule:get_uuid()
    , #{ name => ServiceModule:get_name()
       , description => ServiceModule:get_description()
       , module => ServiceModule
       }}.
