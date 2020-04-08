import { FlowGraph, FlowGraphEdge, FlowGraphNode, CompiledFlowGraph } from './flow_graph';
import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { DirectValue } from './direct_value';
import { EnumDirectValue } from './enum_direct_value';

function get_source_blocks(graph: FlowGraph): string[] {
    const sources = [];
    for(const block_id of Object.keys(graph.nodes)) {
        const block = graph.nodes[block_id];
        if (block.data.type === AtomicFlowBlock.GetBlockType()){
            const data = block.data as AtomicFlowBlockData;

            const inputs = data.value.options.inputs;

            // If it has no pulse inputs its a source block
            if (inputs.filter(v => v.type === 'pulse').length === 0) {
                sources.push(block_id);
            }
        }
        else if (block.data.type === DirectValue.GetBlockType()){
            sources.push(block_id);
        }
        else if (block.data.type === EnumDirectValue.GetBlockType()){
            sources.push(block_id);
        }
    }

    return sources;
}

function makes_reachable(conn: FlowGraphEdge, block: FlowGraphNode): boolean {
    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

        const input = data.value.options.inputs[conn.to.input_index];
        return input.type === 'pulse';
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        return true;
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
        return true;
    }
}

function build_index(arr: string[]): { [key:string]: boolean} {
    const index = {};
    for (const element of arr) {
        index[element] = true;
    }

    return index;
}

function set_difference(whole: any[], subset: {[key: string]: boolean}|string[]): any[] {
    const result = [];
    if (subset.map) { // Is an array
        subset = build_index(subset as string[]);
    }

    for (const element of whole) {
        if (!subset[element]) {
            result.push(element);
        }
    }

    return result;
}

export function get_unreachable(graph: FlowGraph): string[] {
    const reached = build_index(get_source_blocks(graph));

    let remaining_connections: FlowGraphEdge[] = [].concat(graph.edges);
    let empty_pass = false;

    do {
        empty_pass = true;

        const skipped: FlowGraphEdge[] = [];
        for (const conn of remaining_connections) {
            if (reached[conn.from.id]) {
                // Connection activated
                if (makes_reachable(conn, graph.nodes[conn.to.id])) {
                    empty_pass = false;
                    reached[conn.to.id] = true;
                }
            }
            else {
                skipped.push(conn);
            }
        }

        remaining_connections = skipped;
    } while (!empty_pass);

    return set_difference(Object.keys(graph.nodes), reached);
}

export function compile(graph: FlowGraph): CompiledFlowGraph {
    throw new Error("NOT IMPLEMENTED");
}
