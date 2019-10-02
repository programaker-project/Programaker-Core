%%%-------------------------------------------------------------------
%% @doc automate storage configuration and versioning.
%% @end
%%%-------------------------------------------------------------------

-module(automate_storage_versioning).

-export([ apply_versioning/3
        , create_database/2
        , get_database_version/1
        ]).

-include("./versioning.hrl").
-include("./databases.hrl").
-include("./records.hrl").

-spec apply_versioning(#database_version_progression{}, [node()], atom()) -> ok | {error, atom}.
apply_versioning(#database_version_progression{base=Base, updates=Updates}, Nodes, ModuleName) ->
    ok = check_updates_integrity(Updates),

    DBNames = lists:map(fun(DBData = #database_version_data{ database_name=DBName }) ->
                                io:fwrite("[~p] Checking base database: ~p~n", [ModuleName, DBName]),
                                create_database(DBData, Nodes),
                                DBName
                        end, Base),

    ok = mnesia:wait_for_tables(DBNames, automate_configuration:get_table_wait_time()),

    {ok, CurrentDatabaseVersion} = get_database_version(ModuleName),

    ok = apply_updates_after_version(CurrentDatabaseVersion, Updates, ModuleName).


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

-spec get_database_version(atom()) -> {ok, non_neg_integer()}.
get_database_version(ModuleName) ->
    Transaction = fun() ->
                          case mnesia:read(?INSTALLATION_CONFIGURATION_TABLE, {db_version, ModuleName}) of
                              [] ->
                                  0;
                              [#storage_configuration_entry{value=Value}] ->
                                  Value
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Value } ->
            {ok, Value};
        {aborted, {no_exists, ?INSTALLATION_CONFIGURATION_TABLE}} ->
            %% No configuration yet, set to 0
            {ok, 0};
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, Reason}
    end.


-spec set_database_version(atom(), pos_integer()) -> ok.
set_database_version(ModuleName, VersionNumber) ->
    Entry = #storage_configuration_entry{ id={db_version, ModuleName}
                                        , value=VersionNumber
                                        },
    Transaction = fun() ->
                          ok = mnesia:write(?INSTALLATION_CONFIGURATION_TABLE, Entry, write)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Value } ->
            Value;
        { aborted, Reason } ->
            io:format("Error: ~p~n", [mnesia:error_description(Reason)]),
            {error, Reason}
    end.


apply_updates_after_version(_, [], _) ->
    ok;

apply_updates_after_version(OldDatabaseVersion, [#database_version_transformation{ id=Id }
                                                 | T], ModuleName)  when Id =< OldDatabaseVersion ->
    io:fwrite("[~p] Skipping update ~p (current: ~p)~n", [ModuleName, Id, OldDatabaseVersion]),
    apply_updates_after_version(OldDatabaseVersion, T, ModuleName);


apply_updates_after_version(OldDatabaseVersion, [#database_version_transformation{ id=Id
                                                                                 , apply=Fun
                                                                                 }
                                                 | T], ModuleName)  when Id > OldDatabaseVersion ->
    io:fwrite("[~p] APPLYing update ~p (current: ~p)~n", [ModuleName, Id, OldDatabaseVersion]),
    Fun(),
    set_database_version(ModuleName, Id),
    apply_updates_after_version(Id, T, ModuleName).


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
