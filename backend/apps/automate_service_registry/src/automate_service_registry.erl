%%%-------------------------------------------------------------------
%% @doc automate_service_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry).

%% API
-export([ get_all_services/0
        , register_public/1
        , register_private/1
        ]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
get_all_services() ->
    automate_service_registry_mnesia_backend:list_all_public().

-spec register_public(module()) -> {ok, binary()}.
register_public(ServiceModule) ->
    Uuid = ServiceModule:get_uuid(),
    ok = automate_service_registry_mnesia_backend:register(Uuid,
                                                           true, %% Public
                                                           #{ name => ServiceModule:get_name()
                                                            , description => ServiceModule:get_description()
                                                            , module => ServiceModule
                                                            }),
    {ok, Uuid}.

-spec register_private(module()) -> {ok, binary()}.
register_private(ServiceModule) ->
    Uuid = ServiceModule:get_uuid(),
    ok = automate_service_registry_mnesia_backend:register(Uuid,
                                                           private, %% Private
                                                           #{ name => ServiceModule:get_name()
                                                            , description => ServiceModule:get_description()
                                                            , module => ServiceModule
                                                            }),
    {ok, Uuid}.

%%====================================================================
%% Internal functions
%%====================================================================
