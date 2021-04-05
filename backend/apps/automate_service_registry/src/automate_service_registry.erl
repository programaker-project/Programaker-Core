%%%-------------------------------------------------------------------
%% @doc automate_service_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry).

%% API
-export([ get_all_public_services/0
        , get_all_services_for_user/1
        , get_service_by_id/1
        , register_public/1
        , register_private/1
        , update_service_module/3
        , allow_user/2
        , update_visibility/2

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
-spec get_all_public_services() -> {ok, service_info_map()}.
get_all_public_services() ->
    ?BACKEND:list_all_public().

-spec get_all_services_for_user(owner_id()) -> {ok, service_info_map()}.
get_all_services_for_user(Owner) ->
    ?BACKEND:get_all_services_for_user(Owner).

-spec get_service_by_id(binary()) -> {ok, service_entry()} | {error, not_found}.
get_service_by_id(ServiceId) ->
    ?BACKEND:get_service_by_id(ServiceId).

-spec register_public(module() | ?MODULE_MAP) -> {ok, binary()}.
register_public(ServiceModule) ->
    {Uuid, Data} = module_to_map(ServiceModule),
    ok = ?BACKEND:register(Uuid,
                           true, %% Public
                           Data),
    {ok, Uuid}.

-spec update_service_module(module() | ?MODULE_MAP,
                            binary(), owner_id()) -> ok.
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

-spec allow_user(binary(), owner_id()) -> ok | {error, service_not_found}.
allow_user(ServiceId, Owner) ->
    ?BACKEND:allow_user(ServiceId, Owner).

-spec update_visibility(binary(), boolean()) -> ok | {error, service_not_found}.
update_visibility(ServiceId, IsPublic) ->
    ?BACKEND:update_visibility(ServiceId, IsPublic).

-spec get_config_for_service(binary(), atom()) -> {ok, any()} | {error, not_found}.
get_config_for_service(ServiceId, Property) ->
    ?BACKEND:get_config_for_service(ServiceId, Property).

-spec set_config_for_service(binary(), atom(), any()) -> ok | {error, atom()}.
set_config_for_service(ServiceId, Property, Value) ->
    ?BACKEND:set_config_for_service(ServiceId, Property, Value).

-spec count_all_services() -> number().
count_all_services() ->
    ?BACKEND:count_all_services().

-spec delete_service(owner_id(), binary()) -> ok.
delete_service(Owner, ServiceId) ->
    ?BACKEND:delete_service(Owner, ServiceId).



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
