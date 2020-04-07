import { FlowBlockData, Position2D } from "./flow_block";

export interface FlowGraphEdge {
    from: {id: string, output_index: number},
    to: {id: string, input_index: number},
};

export interface FlowGraphNode {
    data: FlowBlockData,
    position: Position2D,
};

export interface FlowGraph {
    nodes: { [key: string]: FlowGraphNode },
    edges: FlowGraphEdge[],
};
