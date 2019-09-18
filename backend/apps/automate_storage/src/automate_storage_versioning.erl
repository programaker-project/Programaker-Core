%%%-------------------------------------------------------------------
%% @doc automate storage configuration and versioning.
%% @end
%%%-------------------------------------------------------------------

-module(automate_storage_versioning).

-exports([ apply_versioning/1
         ]).

-spec apply_versioning(#database_version_progression{}) -> ok | {error, atom}.
apply_versioning(#database_version_progression{}) ->
    ok.
