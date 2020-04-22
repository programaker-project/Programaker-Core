import { FlowGraph, FlowGraphEdge } from './flow_graph';

export function index_connections(graph: FlowGraph): {[key: string]:FlowGraphEdge[]} {
    const index = {};
    for (const conn of graph.edges) {
        if (!index[conn.from.id]) {
            index[conn.from.id] = [];
        }
        index[conn.from.id].push(conn);
    }

    return index;
}

export function reverse_index_connections(graph: FlowGraph): {[key: string]:FlowGraphEdge[]} {
    const index = {};
    for (const conn of graph.edges) {
        if (!index[conn.to.id]) {
            index[conn.to.id] = [];
        }
        index[conn.to.id].push(conn);
    }

    return index;
}
