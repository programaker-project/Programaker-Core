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
    key: "utc_time" | "utc_date";
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
    | "control_wait_for_next_value"
    | "control_if_else"
    | "operator_and" | "operator_equals" | "operator_lt" | "operator_gt"
    | "operator_add" | "operator_modulo"
    | "flow_last_value"
    | "data_setvariableto" | "data_variable"
    | "command_call_service"
    | "control_wait"
    | "logging_add_log" | "flow_get_thread_id"
    | "jump_to_position"
    | "jump_to_block"
    | "op_fork_execution"
    | "trigger_when_all_completed"
    | "trigger_when_first_completed"
    | "op_preload_getter"
    | "data_lengthoflist" | "data_deleteoflist" | "data_addtolist"

// Not found on executable stage, will be removed in link phase
    | "jump_point"

// Operations should not appear on a properly compiled AST
    | "trigger_when_all_completed" | "trigger_when_first_completed"
    | "trigger_on_signal" | "trigger_when_all_true"
    ;
export type CompiledBlockArgs = CompiledBlockArgMonitorDict | CompiledBlockArgCallServiceDict | CompiledBlockArgList;

export interface ContentBlock {
    contents: (CompiledBlock | ContentBlock)[],
};

export interface CompiledBlock {
    id?: string,
    type: CompiledBlockType,
    args?: CompiledBlockArgs,
    contents?: (CompiledBlock | ContentBlock)[],
};

export type CompiledFlowGraph = CompiledBlock[];
