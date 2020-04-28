import { FlowGraph, FlowGraphEdge } from './flow_graph';

export interface IndexedFlowGraphEdge extends FlowGraphEdge {
    index: number,
};

export type EdgeIndex = {[key: string]:IndexedFlowGraphEdge[]};

export function index_connections(graph: FlowGraph): EdgeIndex {
    const index = {};

    let idx = -1;
    for (const conn of graph.edges) {
        idx++;

        if (!index[conn.from.id]) {
            index[conn.from.id] = [];
        }
        index[conn.from.id].push(Object.assign({ index: idx }, conn));
    }

    return index;
}

export function reverse_index_connections(graph: FlowGraph): EdgeIndex {
    const index = {};
    let idx = -1;
    for (const conn of graph.edges) {
        idx++;

        if (!index[conn.to.id]) {
            index[conn.to.id] = [];
        }
        index[conn.to.id].push(Object.assign({ index: idx }, conn));
    }

    return index;
}
