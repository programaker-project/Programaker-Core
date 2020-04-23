import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { FlowGraph, FlowGraphEdge, FlowGraphNode } from './flow_graph';
import { index_connections, reverse_index_connections, IndexedFlowGraphEdge, EdgeIndex } from './graph_utils';
import { FlowEditorComponent } from './flow-editor.component';

function graph_scan_nodes(graph: FlowGraph, check: (node_id: string, node: FlowGraphNode) => boolean): string[] {
    const results = [];
    for (const node_id of Object.keys(graph.nodes)) {
        if (check(node_id, graph.nodes[node_id])) {
            results.push(node_id);
        }
    }

    return results;
}

function graph_scan_atomic_nodes(graph: FlowGraph, check: (node_id: string, node: AtomicFlowBlockData) => boolean): string[] {
    const atomic_block_type = AtomicFlowBlock.GetBlockType();

    return graph_scan_nodes(graph, (node_id: string, node: FlowGraphNode) => {
        if (node.data.type === atomic_block_type) {
            return check(node_id, node.data as AtomicFlowBlockData);
        }
        else {
            return false;
        }
    })
}

type FindCommand = 'continue' | 'stop' | 'capture';

function get_paths_between(_graph: FlowGraph,
                           upper_id: string,
                           lower_id: string,
                           conn_index: {[key: string]: IndexedFlowGraphEdge[]}): [number, number][] {
    const results = [];

    for (const top_conn of conn_index[upper_id] || []) {
        const reached = {};
        reached[upper_id] = true;

        function _aux(descending_id: string, followed_conn_id: number, reached: {[key:string]: boolean}) {
            if (descending_id === lower_id) {
                results.push([top_conn.index, followed_conn_id]);
                return;
            }

            for (const conn of conn_index[descending_id] || []) {
                if (reached[conn.to.id]) {
                    // Already explored, ignoring
                    continue;
                }

                reached[conn.to.id] = true;
                _aux(conn.to.id, conn.index, Object.assign({}, reached));
            }
        }

        _aux(top_conn.to.id, top_conn.index, reached);
    }

    return results;
}

function find_downstream(graph: FlowGraph, source_id: string,
                         conn_index: EdgeIndex,
                         controller: (node_id: string, node: FlowGraphNode) => FindCommand): string[] {
    const reached = {};
    const results = {};
    reached[source_id] = true;

    function _aux(source_id: string) {
        for (const conn of conn_index[source_id] || []) {
            const next_id = conn.to.id;
            if (reached[next_id]) {
                // Ignore repeated
                continue;
            }

            reached[source_id] = true;

            const command = controller(next_id, graph.nodes[next_id]);
            if (command === 'continue') {
                _aux(next_id);
            }
            else if (command === 'stop') {
                // Ignore
            }
            else if (command === 'capture') {
                results[next_id] = true;
            }
            else {
                throw new Error('Unknown "find" command: ' + command)
            }
        }
    }

    _aux(source_id);
    return Object.keys(results);
}

function find_upstream(graph: FlowGraph, source_id: string,
                       rev_conn_index: EdgeIndex,
                       controller: (node_id: string, node: FlowGraphNode) => FindCommand): string[] {
    const reached = {};
    const results = {};
    reached[source_id] = true;

    function _aux(source_id: string) {
        for (const conn of rev_conn_index[source_id] || []) {
            const next_id = conn.from.id;
            if (reached[next_id]) {
                // Ignore repeated
                continue;
            }

            reached[source_id] = true;

            const command = controller(next_id, graph.nodes[next_id]);
            if (command === 'continue') {
                _aux(next_id);
            }
            else if (command === 'stop') {
                // Ignore
            }
            else if (command === 'capture') {
                results[next_id] = true;
            }
            else {
                throw new Error('Unknown "find" command: ' + command)
            }
        }
    }

    _aux(source_id);
    return Object.keys(results);
}

function atomic_find_filter(controller: (node_id: string, node: AtomicFlowBlockData) => FindCommand
                           ): (node_id: string, node: FlowGraphNode) => FindCommand {
    const atomic_block_type = AtomicFlowBlock.GetBlockType();

    return (node_id: string, node: FlowGraphNode) => {
        if (node.data.type === atomic_block_type) {
            return controller(node_id, node.data as AtomicFlowBlockData);
        }
        else {
            return 'continue'; // Ignore
        }
    }
}

function find_downstream_atomic(graph: FlowGraph, source_id: string,
                                conn_index: EdgeIndex,
                                controller: (node_id: string, node: AtomicFlowBlockData) => FindCommand): string[] {
    return find_downstream(graph, source_id, conn_index, atomic_find_filter(controller));
}

function find_upstream_atomic(graph: FlowGraph, source_id: string,
                              rev_conn_index: EdgeIndex,
                              controller: (node_id: string, node: AtomicFlowBlockData) => FindCommand): string[] {
    return find_upstream(graph, source_id, rev_conn_index, atomic_find_filter(controller));
}

function find_forks(graph: FlowGraph): string[] {
    return graph_scan_atomic_nodes(graph, (_node_id: string, node: AtomicFlowBlockData) => {
        return node.value.options.block_function === 'op_fork_execution';
    });
}

function find_downstream_joins(graph: FlowGraph, source_id: string, conn_index: EdgeIndex): string[] {
    return find_downstream_atomic(graph, source_id, conn_index,
                                  (_node_id: string, node: AtomicFlowBlockData) => {
                                      if (node.value.options.block_function === 'trigger_when_any_completed'
                                          || node.value.options.block_function === 'trigger_when_all_completed') {
                                          return 'capture';
                                      }
                                      else {
                                          return 'continue';
                                      }
                                  });
}

function find_upstream_forks(graph: FlowGraph, source_id: string, rev_conn_index: EdgeIndex): string[] {
    return find_upstream_atomic(graph, source_id, rev_conn_index,
                                (_node_id: string, node: AtomicFlowBlockData) => {
                                    if (node.value.options.block_function === 'op_fork_execution') {
                                        return 'capture';
                                    }
                                    else {
                                        return 'continue';
                                    }
                                });
}

function next_empty_fork_output_index(graph: FlowGraph, fork_id: string, conn_index: EdgeIndex): number {
    const filled_outputs = [];
    for (const conn of conn_index[fork_id] || []) {
        filled_outputs[conn.from.output_index] = true;
    }

    let i = 0;
    while (filled_outputs[i]) {
        i++;
    }

    return i;
}

export function lift_common_ops(graph: FlowGraph): FlowGraph {
    let updated = false;

    do {
        let conn_index = index_connections(graph);
        let rev_conn_index = reverse_index_connections(graph);

        updated = false;

        const forks = find_forks(graph);
        for (const fork_id of forks) {
            const next_downstream_joins = find_downstream_joins(graph, fork_id, conn_index);
            if (next_downstream_joins.length > 1) {
                const upstream_forks = find_upstream_forks(graph, fork_id, rev_conn_index);

                if (upstream_forks.length === 0) {
                    continue; // Not the right fork level, try with another
                }

                for (const up_fork_id of upstream_forks) {
                    const paths = get_paths_between(graph, up_fork_id, fork_id, conn_index);
                    if (paths.length === 0) {
                        throw new Error(`AssertionError: Expected path between forks (${up_fork_id} -> ${fork_id})`);
                    }

                    if (paths.length > 1) {
                        throw new Error('NOT IMPLEMENTED: Add fork when lifting')
                    }

                    for (const fork_input of rev_conn_index[up_fork_id]) {
                        // Connect paths leading to upper fork to the moved path
                        const prev_idx = fork_input.index;
                        graph.edges[prev_idx].to = graph.edges[paths[0][0]].to;
                    }

                    let edge_indexes_to_delete = [];
                    for (const fork_output of conn_index[up_fork_id]) {
                        // Connect paths out of the upper fork to the bottom one
                        if (fork_output.index === paths[0][0]) {
                            // Ignore input of the moved path
                            edge_indexes_to_delete.push(fork_output.index);
                        }
                        else {
                            graph.edges[fork_output.index].from = {
                                id: fork_id,
                                output_index: next_empty_fork_output_index(graph, fork_id, conn_index),
                            }

                            if (!conn_index[fork_id]) {
                                conn_index[fork_id] = [];
                            }

                            // Just keep track of the new connections on the fork, to be able to call `next_empty_fork_output_index`
                            conn_index[fork_id].push(
                                Object.assign({index: fork_output.index}, graph.edges[fork_output.index]) as IndexedFlowGraphEdge
                            );
                        }
                    }

                    // Go through the edge-to-remove list in descending order to
                    // avoid keeping track of the index changes
                    for (const edge of edge_indexes_to_delete.sort((a, b) => b - a)) {
                        graph.edges.splice(edge, 1);
                    }
                    delete graph.nodes[up_fork_id];

                    // TODO: Might have to recompute conn_index & rev_conn_index
                    // We don't do this now as it's going back to the top of the
                    // loop and they'll get recomputed anyway.
                }
                updated = true;
                break; // Moved things around, better to restart from a cleaner state
            }
        }
    } while (updated);
    return graph;
}
