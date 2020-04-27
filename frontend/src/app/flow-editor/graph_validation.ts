import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { FlowGraph, FlowGraphEdge, FlowGraphNode } from './flow_graph';
import { get_unreachable } from './graph_analysis';
import { index_connections, reverse_index_connections, EdgeIndex } from './graph_utils';
import { find_downstream } from './graph_transformations';

const LOOP_FOUND = '__loop_found__';

function get_edges_for_nodes(graph: FlowGraph, nodes: {[key:string]: FlowGraphNode }): FlowGraphEdge[] {
    const edges: FlowGraphEdge[] = [];
    for (const conn of graph.edges) {
        if (nodes[conn.from.id] && nodes[conn.to.id]) {
            edges.push(conn);
        }
    }
    return edges;
}

function get_streaming_section(graph: FlowGraph): FlowGraph {
    const nodes: {[key:string]: FlowGraphNode } = {};
    for (const block_id of Object.keys(graph.nodes)) {
        const block = graph.nodes[block_id];
        if (block.data.type === AtomicFlowBlock.GetBlockType()){
            const data = block.data as AtomicFlowBlockData;

            const inputs = data.value.options.inputs || [];
            const outputs = data.value.options.outputs || [];

            // If it has no pulse inputs or outputs its a streaming block
            if ((inputs.filter(v => v.type === 'pulse').length === 0)
                && (outputs.filter(v => v.type === 'pulse').length === 0)) {
                nodes[block_id] = block;
            }
        }
        else {
            nodes[block_id] = block;
        }
    }


    return {
        nodes: nodes,
        edges: get_edges_for_nodes(graph, nodes),
    };
}

function get_stepped_section(graph: FlowGraph): FlowGraph {
    const nodes: {[key:string]: FlowGraphNode } = {};
    for (const block_id of Object.keys(graph.nodes)) {
        const block = graph.nodes[block_id];
        if (block.data.type === AtomicFlowBlock.GetBlockType()){
            const data = block.data as AtomicFlowBlockData;

            const inputs = data.value.options.inputs || [];
            const outputs = data.value.options.outputs || [];

            // If it has no pulse inputs or outputs its a streaming block
            if ((inputs.filter(v => v.type === 'pulse').length > 0)
                || (outputs.filter(v => v.type === 'pulse').length > 0)) {
                nodes[block_id] = block;
            }
        }
    }

    return {
        nodes: nodes,
        edges: get_edges_for_nodes(graph, nodes),
    };
}

function validate_streaming_no_loop_around(_graph: FlowGraph,
                                           connections_index: {[key: string]:FlowGraphEdge[]},
                                           block_id: string): {[key:string]: boolean} {

    function aux(block_id:string, top: {[key:string]: boolean}): {[key:string]: boolean} {
        const reached = {};
        reached[block_id] = true ;

        for (const conn of connections_index[block_id] || []) {
            if (top[conn.to.id]) {
                throw new Error(`ValidationError: Loop in streaming section around block (id=${conn.to.id})`)
            }

            const col_top = {}
            col_top[conn.to.id] = true;
            Object.assign(col_top, top);

            const reached_in_col = aux(conn.to.id, col_top);
            Object.assign(reached, reached_in_col);
        }

        return reached;
    }

    const top = {};
    top[block_id] = true ;
    const reached = aux(block_id, top);

    return reached;
}

function validate_no_loops_in_streaming_section(graph: FlowGraph) {
    const streaming_graph = get_streaming_section(graph);

    const connections_index = index_connections(streaming_graph);
    const validated: {[key:string]: boolean} = {};

    for (const block_id of Object.keys(streaming_graph.nodes)) {
        if (!validated[block_id]) {
            const validated_group = validate_streaming_no_loop_around(streaming_graph, connections_index, block_id)
            Object.assign(validated, validated_group);
        }
    }
}

function validate_that_all_paths_have_fork(graph: FlowGraph,
                                           join_bottom_id: string,
                                           conn_index: {[key: string]:FlowGraphEdge[]}) {

    function try_find_upwards_without_fork(bottom_id: string, depth: number, reached: {[key: string]: boolean},
                                           acc: { control_if: string[]  }): string {
        const block = graph.nodes[bottom_id];
        let control_if_acc = acc.control_if;

        if (block.data.type === AtomicFlowBlock.GetBlockType()) {
            const a_block = block.data as AtomicFlowBlockData;
            if (a_block.value.options.block_function === "op_fork_execution") {
                return null; // This path is not problematic
            }
            else if (a_block.value.options.block_function === "control_if_else") {
                if (control_if_acc) {
                    control_if_acc.push(bottom_id);
                }
            }
            else if (a_block.value.options.block_function === "trigger_when_first_completed") {
                // This disables the problem related to the control_if accumulator
                control_if_acc = null;
            }
        }

        const upwards = conn_index[bottom_id];
        if (!upwards || !upwards.length) {
            return bottom_id; // Found a problematic path
        }

        reached[bottom_id] = true;

        let conn_out_of_loop = 0;
        for (const conn of upwards) {
            if (reached[conn.from.id]) {
                continue; // Skip if already reached
            }

            const col_reached = Object.assign({}, reached);
            try {
                const source = try_find_upwards_without_fork(conn.from.id, depth + 1, col_reached, { control_if: control_if_acc });
                if (source) {
                    return source;
                }
                conn_out_of_loop += 1;
            }
            catch (err) {
                // This is horribly inefficient, as the exception will be thrown
                // and catched >3000 times.
                //
                // As is now, this should only happen when a infinite loop is
                // found during the validation of the graph. This would signal a
                // error on the way to work of the function. In that case we can
                // pay a performance price in exchange of getting as much
                // information as possible. This should NEVER happen on real
                // usage of the code, and at most when writing new tests for
                // problematic data.
                if (err.message === 'Maximum call stack size exceeded') {
                    err.message = `[Depth ${depth}] ${err.message}. Maybe there's an unmanaged loop?`;
                }
                throw err;
            }
        }

        if (conn_out_of_loop === 0) {
            return bottom_id; // Cannot get to a FORK even if all paths are explored
        }
        return null;
    }

    const known_if_blocks = {};

    for (const conn of conn_index[join_bottom_id] ) {
        const reached = {};
        reached[conn.to.id] = true;

        const acc = { control_if: [] };
        const source = try_find_upwards_without_fork(conn.from.id, 1, reached, acc);
        if (source) {
            throw new Error(`ValidationError: Block (id:${source}) can get to Join (id:${join_bottom_id}) with no fork.`
                            + ' Joins can only be done between flows that have previously forked.');
        }

        // Check that no two connections lead to the same IF
        for (const block of acc.control_if) {
            if (known_if_blocks[block]) {
                throw new Error(`ValidationError: A single conditional block (id:${block}) has two connections to a fork join block.`
                    + ' From an IF block only one connection can be established to the Join block.'
                    + ' Consider merging the conditional paths using a `trigger_when_first_completed` block.');
            }
            known_if_blocks[block] = true;
        }
    }
}

function validate_joins_only_after_forks(graph: FlowGraph) {
    const stepped_graph = get_stepped_section(graph);
    const connections_index = reverse_index_connections(stepped_graph);
    for (const block_id of Object.keys(stepped_graph.nodes)) {
        const block = stepped_graph.nodes[block_id];

        if (block.data.type === AtomicFlowBlock.GetBlockType()) {
            const a_block = block.data as AtomicFlowBlockData;
            if (a_block.value.options.block_function === "trigger_when_all_completed") {
                validate_that_all_paths_have_fork(stepped_graph, block_id, connections_index);
            }
        }
    }
}

function validate_no_loops_around_block(graph: FlowGraph, block_id: string, conn_index: EdgeIndex) {
    // Look for the block_id starting from each connection
    for (const conn of conn_index[block_id]) {
        find_downstream(graph, conn.to.id, conn_index, (node_id: string, _node: FlowGraphNode) => {
            if (node_id === block_id) {
                throw LOOP_FOUND;
            }

            return 'continue';
        });
    }
}

function validate_jumps_not_out_of_forks(graph: FlowGraph) {
    const stepped_graph = get_stepped_section(graph);
    const connections_index = index_connections(stepped_graph);
    for (const block_id of Object.keys(stepped_graph.nodes)) {
        const block = stepped_graph.nodes[block_id];

        if (block.data.type === AtomicFlowBlock.GetBlockType()) {
            const a_block = block.data as AtomicFlowBlockData;
            if (a_block.value.options.block_function === "op_fork_execution") {
                try {
                    validate_no_loops_around_block(stepped_graph, block_id, connections_index);
                }
                catch (err) {
                    if (err === LOOP_FOUND) {
                        throw new Error('ValidationError: Loop around Fork blocks not allowed.'
                                        + ` Found around block (id:${block_id})`)
                    }
                    else {
                        throw err;
                    }
                }
            }
        }
    }
}

export function validate(graph: FlowGraph) {
    // Reject loops in streaming section
    validate_no_loops_in_streaming_section(graph);
    const unreachable = get_unreachable(graph);
    if (unreachable.length > 0) {
        throw new Error(`ValidationError: Unreachable blocks (${unreachable})`);
    }
    validate_joins_only_after_forks(graph);
    validate_jumps_not_out_of_forks(graph);

    return true;
}
