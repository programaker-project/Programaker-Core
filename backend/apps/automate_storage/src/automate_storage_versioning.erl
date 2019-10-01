%%%-------------------------------------------------------------------
%% @doc automate storage configuration and versioning.
%% @end
%%%-------------------------------------------------------------------

-module(automate_storage_versioning).

-export([ apply_versioning/2
        , create_database/2
        ]).

-include("./versioning.hrl").
-include("./databases.hrl").

-spec apply_versioning(#database_version_progression{}, [node()]) -> ok | {error, atom}.
apply_versioning(#database_version_progression{base=Base, updates=Updates}, Nodes) ->
    ok = check_updates_integrity(Updates),

    DBNames = lists:map(fun(DBData = #database_version_data{ database_name=DBName }) ->
                                create_database(DBData, Nodes),
                                DBName
                        end, Base),

    ok = mnesia:wait_for_tables(DBNames, automate_configuration:get_table_wait_time()),

    CurrentDatabaseVersion = get_database_version(),

    ok = apply_updates_after_version(CurrentDatabaseVersion, Updates).


-spec create_database(#database_version_data{}, [node()]) -> ok.
create_database(#database_version_data{ database_name=DBName
                                      , records=Fields
                                      , record_name=RecordName
                                      }, Nodes) ->
    case mnesia:create_table(DBName,
                             [ {attributes, Fields}
                             , { disc_copies, Nodes }
                             , { record_name, RecordName }
                             , { type, set }
                             ]) of
        { atomic, ok } ->
            ok;
        { aborted, { already_exists, _ }} ->
            ok
    end.


get_database_version() ->
    %% @WIP: Write code to extract database version.
    0.

set_database_version(VersionNumber) ->
    %% @WIP: Write code to set database version.
    ok.


apply_updates_after_version(_, []) ->
    ok;

apply_updates_after_version(OldDatabaseVersion, [#database_version_transformation{ id=Id }
                                                 | T])  when Id =< OldDatabaseVersion ->
    apply_updates_after_version(OldDatabaseVersion, T);


apply_updates_after_version(OldDatabaseVersion, [#database_version_transformation{ id=Id
                                                                                 , apply=Fun
                                                                                 }
                                                | T])  when Id > OldDatabaseVersion ->
    Fun(),
    set_database_version(Id),
    apply_updates_after_version(Id, T).


check_updates_integrity(Updates) ->
    check_updates_integrity(0, Updates).

check_updates_integrity(_, []) ->
    ok;
check_updates_integrity(MinVersionLessOne, [ Update=#database_version_transformation{ id=Id
                                                                                    , apply=Fun
                                                                                    }
                                             | T ]) when is_function(Fun) ->
    if Id < MinVersionLessOne ->
            {error, version_number_not_increasing, Update};
       true ->
            check_updates_integrity(Id, T)
    end.
