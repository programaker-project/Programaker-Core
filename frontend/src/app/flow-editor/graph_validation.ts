import { FlowGraph, FlowGraphNode, FlowGraphEdge } from './flow_graph';
import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { get_unreachable } from './graph_analysis';

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

    const edges: FlowGraphEdge[] = [];
    for (const conn of graph.edges) {
        if (nodes[conn.from.id] && nodes[conn.to.id]) {
            edges.push(conn);
        }
    }

    return {
        nodes: nodes,
        edges: edges,
    };
}

function validate_streaming_loop_around(graph: FlowGraph,
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

function index_connections(graph: FlowGraph): {[key: string]:FlowGraphEdge[]} {
    const index = {};
    for (const conn of graph.edges) {
        if (!index[conn.from.id]) {
            index[conn.from.id] = [];
        }
        index[conn.from.id].push(conn);
    }

    return index;
}

function validate_loops_in_streaming_section(graph: FlowGraph) {
    const streaming_graph = get_streaming_section(graph);

    const connections_index = index_connections(streaming_graph);
    const validated: {[key:string]: boolean} = {};

    for (const block_id of Object.keys(streaming_graph.nodes)) {
        if (!validated[block_id]) {
            const validated_group = validate_streaming_loop_around(streaming_graph, connections_index, block_id)
            Object.assign(validated, validated_group);
        }
    }
}

export function validate(graph: FlowGraph) {
    // Reject loops in streaming section
    validate_loops_in_streaming_section(graph);
    const unreachable = get_unreachable(graph);
    if (unreachable.length > 0) {
        throw new Error(`Validation error: Unreachable blocks (${unreachable})`);
    }

    return true;
}
