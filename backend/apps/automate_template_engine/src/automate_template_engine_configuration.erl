%%%-------------------------------------------------------------------
%% @doc automate template engine configuration and versioning
%% @end
%%%-------------------------------------------------------------------
-module(automate_template_engine_configuration).

-export([ get_versioning/1
        ]).

-include("databases.hrl").
-include("../../automate_storage/src/versioning.hrl").

-spec get_versioning([node()]) -> #database_version_progression{}.
get_versioning(_Nodes) ->
    %% Service port identity table
    Version_1 = [ #database_version_data{ database_name=?TEMPLATE_TABLE
                                        , records=[ id, name, owner, content ]
                                        , record_name=template_entry
                                        }
                ],

    #database_version_progression
        { base=Version_1
        , updates=
              %% Introduce user groups
              [ #database_version_transformation
                { id=1
                , apply=fun() ->
                                {atomic, ok} = mnesia:transform_table(
                                                 ?TEMPLATE_TABLE,
                                                 fun({ template_entry
                                                     , Id, Name, Owner, Content
                                                     }) ->
                                                         { template_entry
                                                         , Id, Name, {user, Owner}, Content
                                                         }
                                                 end,
                                                 [ id, name, owner, content
                                                 ],
                                                 template_entry
                                                ),

                                ok = mnesia:wait_for_tables([ ?TEMPLATE_TABLE ],
                                                            automate_configuration:get_table_wait_time())

                        end
                }
              ]
        }.
