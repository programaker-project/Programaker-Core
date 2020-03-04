-module(automate_service_port_engine_stats).

-export([ get_bridge_metrics/0
        ]).


-include("./databases.hrl").
-include("./records.hrl").

-spec get_bridge_metrics() -> { ok
                              , non_neg_integer(), non_neg_integer()
                              , non_neg_integer(), non_neg_integer()
                              , non_neg_integer()
                              }.
get_bridge_metrics() ->
    Transaction = fun() ->
                          %% Bridges
                          PublicMatchHead = #service_port_configuration{ id='_'
                                                                       , service_name='_'
                                                                       , service_id='_'
                                                                       , is_public='$1'
                                                                       , blocks='_'
                                                                       , icon='_'
                                                                       , allow_multiple_connections='_'
                                                                       },
                          PublicMatcher = [{ PublicMatchHead
                                           , [{ '==', '$1', true }]
                                           , ['_']}],
                          PrivateMatcher = [{ PublicMatchHead
                                            , [{ '==', '$1', false }]
                                            , ['_']}],

                          NumBridgesPublic = select_length(?SERVICE_PORT_CONFIGURATION_TABLE, PublicMatcher),
                          NumBridgesPrivate = select_length(?SERVICE_PORT_CONFIGURATION_TABLE, PrivateMatcher),

                          %% Connections and messages
                          NumConnections = mnesia:table_info(?CONNECTED_BRIDGES_TABLE, size),
                          OnFlightMessages = mnesia:table_info(?ON_FLIGHT_MESSAGES_TABLE, size),

                          ConnectionMatchHead = #bridge_connection_entry{ id='$1'
                                                                      , pid='_'
                                                                      , node='_'
                                                                      },
                          ConnectionMatcher = [{ ConnectionMatchHead
                                            , []
                                            , ['$1']}],

                          NumUniqueConnections = select_unique_length(?CONNECTED_BRIDGES_TABLE, ConnectionMatcher),
                          { ok
                          , NumBridgesPublic, NumBridgesPrivate
                          , NumConnections, NumUniqueConnections
                          , OnFlightMessages
                          }
                  end,
    mnesia:async_dirty(Transaction).

%%====================================================================
%% Internal functions
%%====================================================================
select_length(Tab, Matcher) ->
    case mnesia:select(Tab, Matcher) of
        Records ->
            length(Records)
    end.

select_unique_length(Tab, Matcher) ->
    case mnesia:select(Tab, Matcher) of
        Records ->
            Unique = sets:from_list(Records),
            sets:size(Unique)
    end.
