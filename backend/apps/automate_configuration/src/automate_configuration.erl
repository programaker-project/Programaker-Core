%%%-------------------------------------------------------------------
%% @doc automate_configuration module
%% @end
%%%-------------------------------------------------------------------

-module(automate_configuration).

-export([ get_table_wait_time/0
        , get_sync_peers/0
        , get_sync_primary/0
        , is_node_primary/1
        ]).

-define(APPLICATION, automate).
-define(DEFAULT_WAIT_TIME, 10000).

-define(SYNC_PRIMARY_ENV_VARIABLE, "AUTOMATE_SYNC_PRIMARY").
-define(SYNC_PEERS_ENV_VARIABLE, "AUTOMATE_SYNC_PEERS").
-define(SYNC_PEERS_SPLIT_TOKEN, ",").

%%====================================================================
%% Utils functions 
%%====================================================================
-spec get_table_wait_time() -> non_neg_integer().
get_table_wait_time() ->
    application:get_env(?APPLICATION, table_wait_time, ?DEFAULT_WAIT_TIME).

-spec get_sync_peers() -> [node()].
get_sync_peers() ->
    case os:getenv(?SYNC_PEERS_ENV_VARIABLE) of
        false ->
            [];
        Value -> %% peer1@node1,peer2@node2
            lists:filtermap(fun (Candidate) ->
                              case string:trim(Candidate) of
                                  "" -> false;
                                  Node -> {true, list_to_atom(Node)}
                              end
                      end,
                      string:split(Value, ?SYNC_PEERS_SPLIT_TOKEN, all))
    end.

-spec get_sync_primary() -> node() | none.
get_sync_primary() ->
    case os:getenv(?SYNC_PRIMARY_ENV_VARIABLE) of
        false ->
            none;
        Value ->
            list_to_atom(Value)
    end.

-spec is_node_primary(node()) -> boolean().
is_node_primary(Node) ->
    case {node(), automate_configuration:get_sync_primary()} of
        {Node, Node} -> true; %% Is primary
        {_, none} -> true;    %% Primary not specified
        _ -> false            %% Another is specified as primary
    end.

    
