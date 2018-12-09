%%%-------------------------------------------------------------------
%% @doc automate_service_registry API.
%% @end
%%%-------------------------------------------------------------------

-module(automate_service_registry).

%% API
-export([ register/1
        , get_all_services/0
        ]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
register(ServiceModule) ->
    Uuid = ServiceModule:get_uuid(),
    ok = automate_service_registry_mnesia_backend:register(Uuid,
                                                           #{ name => ServiceModule:get_name()
                                                            , description => ServiceModule:get_description()
                                                            , module => ServiceModule
                                                            }),
    {ok, Uuid}.

get_all_services() ->
    automate_service_registry_mnesia_backend:list_all().

%%====================================================================
%% Internal functions
%%====================================================================
