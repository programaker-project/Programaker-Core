-module(automate_configuration_distributed).

%% API exports
-export([ start_link/0
        ]).

%%====================================================================
%% API functions
%%====================================================================
%% Exposed startup entrypoint
start_link() ->
    connect_nodes(automate_configuration:get_sync_peers()),
    ignore.

%%====================================================================
%% Internal functions
%%====================================================================
connect_nodes(Nodes) ->
    lists:foreach(fun (Node) ->
                          net_kernel:connect_node(Node)
                  end, Nodes).
