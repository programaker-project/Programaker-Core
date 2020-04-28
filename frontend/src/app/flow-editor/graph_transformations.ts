import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { FlowGraph, FlowGraphEdge, FlowGraphNode } from './flow_graph';
import { index_connections, reverse_index_connections, IndexedFlowGraphEdge, EdgeIndex } from './graph_utils';
import { FlowEditorComponent } from './flow-editor.component';
import { DirectValue, DirectValueFlowBlockData } from './direct_value';
import { EnumDirectValue } from './enum_direct_value';
import { uuidv4 } from './utils';
import { OP_PRELOAD_BLOCK } from './base_toolbox_description';

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
type ScanCommand = FindCommand;

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

export function find_downstream(graph: FlowGraph, source_id: string,
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

export function find_upstream(graph: FlowGraph, source_id: string,
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

export function scan_upstream(graph: FlowGraph, source_id: string,
                              rev_conn_index: EdgeIndex,
                              controller: (node_id: string, node: FlowGraphNode, path: string[]) => ScanCommand): [string] {
    const reached = {};
    reached[source_id] = true;

    function _aux(source_id: string, path: string[]) {
        for (const conn of rev_conn_index[source_id] || []) {
            const next_id = conn.from.id;
            if (reached[next_id]) {
                // Ignore repeated
                continue;
            }

            reached[source_id] = true;

            const branch_path = path.concat([conn.from.id]);
            const result = controller(next_id, graph.nodes[next_id], branch_path);
            if (result === 'continue') {
                let result = _aux(next_id, branch_path);
                if (result) {
                    return result;
                }
            }
            else if (result === 'stop') {
                // Ignore this branch
            }
            else if (result === 'capture') {
                return branch_path;
            }
            else {
                throw new Error('Unknown "scan" command: ' + result);
            }
        }

    }

    return _aux(source_id, [source_id]);
}

export function scan_downstream(graph: FlowGraph, source_id: string,
                              conn_index: EdgeIndex,
                              controller: (node_id: string, node: FlowGraphNode, path: string[]) => ScanCommand): [string] {
    const reached = {};
    reached[source_id] = true;

    function _aux(source_id: string, path: string[]) {
        for (const conn of conn_index[source_id] || []) {
            const next_id = conn.to.id;
            if (reached[next_id]) {
                // Ignore repeated
                continue;
            }

            reached[source_id] = true;

            const branch_path = path.concat([conn.to.id]);
            const result = controller(next_id, graph.nodes[next_id], branch_path);
            if (result === 'continue') {
                let result = _aux(next_id, branch_path);
                if (result) {
                    return result;
                }
            }
            else if (result === 'stop') {
                // Ignore this branch
            }
            else if (result === 'capture') {
                return branch_path;
            }
            else {
                throw new Error('Unknown "scan" command: ' + result);
            }
        }

    }

    return _aux(source_id, [source_id]);
}

export function is_pulse_output(block: FlowGraphNode, index: number): boolean {
    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;
        if (!data.value.options) {
            throw new Error(`No options found on ${JSON.stringify(block)}`)
        }

        const outputs = data.value.options.outputs;
        if (!outputs[index]) {
            throw new Error(`IndexError: Index (${index}) not found on outputs (${JSON.stringify(outputs)})`)
        }

        // If it has no pulse inputs its a source block
        return outputs[index].type === 'pulse';
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        const data = block.data as DirectValueFlowBlockData;

        return data.value.type === 'pulse';
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
        return false;
    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

export function scan_pulse_upstream(graph: FlowGraph, source_id: string,
                                    rev_conn_index: EdgeIndex,
                                    controller: (node_id: string, node: FlowGraphNode, path: string[]) => ScanCommand): [string] {
    const reached = {};
    reached[source_id] = true;

    function _aux(source_id: string, path: string[]) {
        for (const conn of rev_conn_index[source_id] || []) {
            const next_id = conn.from.id;
            if (reached[next_id] || (!is_pulse_output(graph.nodes[next_id], conn.from.output_index))) {
                // Ignore repeated
                continue;
            }

            reached[source_id] = true;

            const branch_path = path.concat([conn.from.id]);
            const result = controller(next_id, graph.nodes[next_id], branch_path);
            if (result === 'continue') {
                let result = _aux(next_id, branch_path);
                if (result) {
                    return result;
                }
            }
            else if (result === 'stop') {
                // Ignore this branch
            }
            else if (result === 'capture') {
                return branch_path;
            }
            else {
                throw new Error('Unknown "scan" command: ' + result);
            }
        }

    }

    return _aux(source_id, [source_id]);
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
                                      if (node.value.options.block_function === 'trigger_when_first_completed'
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

function get_first_user_per_ast(graph: FlowGraph, getter: string, conn_index: EdgeIndex, rev_conn_index: EdgeIndex): string[] {
    const users = find_downstream_atomic(graph, getter, conn_index,
                                         (_node_id: string, node: AtomicFlowBlockData) => {
                                             if (node.value.options.type !== 'getter') {
                                                 return 'capture';
                                             }
                                             else {
                                                 return 'continue';
                                             }
                                         });

    // Build candidate index
    const candidates = {};
    for (const user of users) {
        candidates[user] = true;
    }

    // For each item in list, check if it has another candidate up.
    // If it has one discard it.
    for (const user of users) {
        const scan_controller = ((node_id: string, node: FlowGraphNode) => {
            if (node.data.type !== AtomicFlowBlock.GetBlockType()) {
                return 'continue';
            }

            const block = node.data as AtomicFlowBlockData;
            if ((node_id !== user) && (candidates[node_id])) {
                return 'stop'; // This path has another candidate, so it's not usable
            }
            else if (block.value.options.type === 'trigger') {
                return 'capture'; // A path to a trigger found with no other candidates before
            }
            else {
                return 'continue';
            }
        });

        if (!scan_pulse_upstream(graph, user, rev_conn_index, scan_controller)) {
            delete candidates[user];
        }
    }

    return Object.keys(candidates);
}

function get_number_of_uses(graph: FlowGraph, operation: string, getter: string, conn_index: EdgeIndex): number {
    let count = 0;

    for (const conn of conn_index[getter]) {
        const scan_controller = ((node_id: string, node: FlowGraphNode) => {
            if (node.data.type !== AtomicFlowBlock.GetBlockType()) {
                throw new Error("Unexpected: found input to non-atomic block");
            }

            const block = node.data as AtomicFlowBlockData;

            if (node_id === operation) {
                return 'capture';
            }
            else if (block.value.options.type !== 'getter') {
                return 'stop';
            }
            else {
                return 'continue';
            }
        });
        if (scan_downstream(graph, conn.to.id, conn_index, scan_controller)) {
            count++;
        }
    }

    return count;
}

export function extract_internally_reused_argument(graph: FlowGraph): FlowGraph {
    const conn_index = index_connections(graph);
    const rev_conn_index = reverse_index_connections(graph);

    // Steps
    //
    // 1. Find all getter blocks with more than one out connection.
    // 2. For each AST path find first block in each AST where is called.
    // 3. If it's used >1 time in that block
    // 4. Invoke before it to set the value (type: cache-value?)

    const getters = graph_scan_atomic_nodes(graph, (_node_id: string, node: AtomicFlowBlockData) => {
        return node.value.options.type === 'getter';
    });

    for (const getter of getters) {
        const ast_tops = get_first_user_per_ast(graph, getter, conn_index, rev_conn_index);
        for (const top of ast_tops) {
            if (get_number_of_uses(graph, top, getter, conn_index) > 1) {

                const ref = uuidv4();

                const [block_options, synth_in, synth_out] = AtomicFlowBlock.add_synth_io(OP_PRELOAD_BLOCK);
                const preload_op = {
                    type: AtomicFlowBlock.GetBlockType(),
                    value: {
                        options: block_options,
                        synthetic_input_count: synth_in,
                        synthetic_output_count: synth_out,
                    }
                };

                graph.nodes[ref] =  { data: preload_op, position: null };

                rev_conn_index[ref] = [];
                conn_index[ref] = [];

                // Introduce preload op before the top of AST
                for (let i = 0; i < rev_conn_index[top].length; i++) {
                    const incoming = rev_conn_index[top][i];

                    if (!is_pulse_output(graph.nodes[incoming.from.id], incoming.from.output_index)) {
                        continue;
                    }

                    if (incoming.to.input_index !== 0) {
                        throw new Error("Unexpected: Moving block to inject preload. Pulse input on port != 0")
                    }

                    for (const conn of conn_index[incoming.from.id]) {
                        if (conn.to.id === top) {
                            conn.to.id = ref;
                        }
                    }

                    incoming.to.id = ref;
                    rev_conn_index[ref].push(incoming);
                    rev_conn_index[top].splice(i, 1);
                    i--;
                }

                // Connect preloader to AST top
                const conn_to_top = {
                    from: { id: ref, output_index: 0 },
                    to: { id: top, input_index: 0 },
                    index: null,
                };

                graph.edges.push(conn_to_top);
                conn_to_top.index = graph.edges.length - 1;

                rev_conn_index[top].push(conn_to_top);
                conn_index[ref].push(conn_to_top);

                // Connect preloader to getter
                const conn_to_arg = {
                    from: { id: getter, output_index: 0 },
                    to: { id: ref, input_index: 1 },
                    index: null,
                };

                graph.edges.push(conn_to_arg);
                conn_to_arg.index = graph.edges.length - 1;

                rev_conn_index[ref].push(conn_to_arg);
                conn_index[getter].push(conn_to_arg);
            }
        }
    }

    return graph;
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
