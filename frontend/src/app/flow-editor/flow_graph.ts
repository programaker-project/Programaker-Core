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

// Compiled graph
export interface CompiledConstantArg {
    type: 'constant',
    value: any,
};

export type MonitorExpectedValue = CompiledConstantArg;

export interface CompiledBlockArgMonitorDict {
    monitor_id: {
        from_service: string,
    },
    expected_value: MonitorExpectedValue | 'any_value';
    save_to?: {
        type: 'variable',
        value: string,
    }
}

export interface CompiledBlockArgCallServiceDict {
    service_id: string,
    service_action: string,
    service_call_values: CompiledBlockArgList,
}

export interface CompiledBlockArgBlock {
    type: 'block',
    value: CompiledBlock[]
}

export type CompiledBlockArg = CompiledBlockArgBlock | CompiledConstantArg;

export type CompiledBlockArgList = CompiledBlockArg[];

export type CompiledBlockType = "wait_for_monitor"
    | "control_if_else"
    | "operator_and" | "operator_equals"
    | "flow_last_value" | "flow_set_value"
    | "command_call_service"
    ;
export type CompiledBlockArgs = CompiledBlockArgMonitorDict | CompiledBlockArgCallServiceDict | CompiledBlockArgList;

export interface ContentBlock {
    contents: (CompiledBlock | ContentBlock)[],
};

export interface CompiledBlock {
    type: CompiledBlockType,
    args?: CompiledBlockArgs,
    contents?: (CompiledBlock | ContentBlock)[],
};

export type CompiledFlowGraph = CompiledBlock[];
