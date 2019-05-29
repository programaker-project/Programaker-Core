-module(automate_configuration_distributed).

%% API exports
-export([ start_link/0
        ]).

-define(SYNC_PEERS_ENV_VARIABLE, "AUTOMATE_SYNC_PEERS").
-define(SYNC_PEERS_SPLIT_TOKEN, ",").

%%====================================================================
%% API functions
%%====================================================================
%% Exposed startup entrypoint
start_link() ->
    case os:getenv(?SYNC_PEERS_ENV_VARIABLE) of
        false ->
            ok;
        Value -> %% peer1@node1,peer2@node2
            ok = connect_nodes(string:split(Value, ?SYNC_PEERS_SPLIT_TOKEN, all))
    end,
    ignore.


%%====================================================================
%% Internal functions
%%====================================================================
connect_nodes(Nodes) ->
    lists:foreach(fun (Node) ->
                          case string:trim(Node) of
                              "" -> ok;
                              NodeName -> net_kernel:connect_node(list_to_atom(NodeName))
                          end
                  end, Nodes).
