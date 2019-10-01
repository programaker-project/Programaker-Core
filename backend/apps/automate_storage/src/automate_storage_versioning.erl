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
apply_versioning(#database_version_progression{base=Base, updates=_Updates}, Nodes) ->
    DBNames = lists:map(fun(DBData = #database_version_data{ database_name=DBName }) ->
                                create_database(DBData, Nodes),
                                DBName
                        end, Base),

    ok = mnesia:wait_for_tables(DBNames, automate_configuration:get_table_wait_time()),

    %% @WIP: Apply updates

    ok.


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
