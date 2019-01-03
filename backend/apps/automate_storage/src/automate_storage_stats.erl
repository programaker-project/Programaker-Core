%% Automate storage extension for stats

-module(automate_storage_stats).

-export([ count_users/0
        ]).


%%====================================================================
%% API functions
%%====================================================================
-spec count_users() -> number().
count_users() ->
    length(mnesia:dirty_all_keys(automate_registered_users)).
