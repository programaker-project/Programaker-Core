import { AtomicFlowBlock, AtomicFlowBlockData, AtomicFlowBlockOptions, isAtomicFlowBlockOptions, isAtomicFlowBlockData } from './atomic_flow_block';
import { BaseToolboxDescription, ToolboxDescription } from './base_toolbox_description';
import { DirectValueFlowBlockData, isDirectValueBlockData } from './direct_value';
import { EnumDirectValueFlowBlockData, isEnumDirectValueBlockData, EnumDirectValue } from './enum_direct_value';
import { CompiledBlock, CompiledBlockArg, CompiledBlockArgs, CompiledFlowGraph, ContentBlock, FlowGraph, FlowGraphEdge, FlowGraphNode, CompiledBlockArgList, CompiledBlockType, CompiledBlockArgMonitorDict, CompiledBlockArgCallServiceDict, CompiledBlockServiceCallSelectorArgs } from './flow_graph';
import { extract_internally_reused_arguments, is_pulse_output, lift_common_ops, scan_downstream, scan_upstream, split_streaming_after_stepped, is_pulse } from './graph_transformations';
import { index_connections, reverse_index_connections, EdgeIndex, IndexedFlowGraphEdge } from './graph_utils';
import { TIME_MONITOR_ID } from './platform_facilities';
import { uuidv4 } from './utils';
import { isUiFlowBlockData } from './ui-blocks/ui_flow_block';

function index_toolbox_description(desc: ToolboxDescription): {[key: string]: AtomicFlowBlockOptions} {
    const result: {[key: string]: AtomicFlowBlockOptions} = {};

    for (const cat of desc) {
        for (const block of cat.blocks) {
            // TODO: This will most probably require UI block definitions too
            if (isAtomicFlowBlockOptions(block)) {
                result[block.block_function] = block;
            }
        }
    }

    return result;
}

const BASE_TOOLBOX_BLOCKS = index_toolbox_description(BaseToolboxDescription);

const JUMP_TO_POSITION_OPERATION = 'jump_to_position';
const JUMP_TO_BLOCK_OPERATION = 'jump_to_block';
const FORK_OPERATION = 'op_fork_execution';
const REPEAT_OPERATION = 'control_repeat';
const TIME_TRIGGERS = ['flow_utc_date', 'flow_utc_time'];

function makes_reachable(conn: FlowGraphEdge, block: FlowGraphNode): boolean {
    if (isAtomicFlowBlockData(block.data)){
        const data = block.data;

        if (data.value.options.type !== 'operation') {
            // Getter or trigger
            return true;
        }

        const input = data.value.options.inputs[conn.to.input_index];
        if (!input) {
            const extras = data.value.options.extra_inputs;
            if (extras) {
                return is_pulse(extras);
            }

            throw new Error(`No input #${conn.to.input_index} on ${JSON.stringify(data.value.options.inputs)} [conn: ${JSON.stringify(conn)}]`);
        }

        return is_pulse(input);
    }
    else if (isDirectValueBlockData(block.data)){
        throw new Error('Connection from reached block to value (backwards?)');
    }
    else if (isEnumDirectValueBlockData(block.data)){
        throw new Error('Connection from reached block to value (backwards?)');
    }
    else if (isUiFlowBlockData(block.data)) {
        return true;
    }
}

function build_index(arr: string[]): { [key:string]: boolean} {
    const index: {[key: string]: boolean} = {};

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

    const subsetMap = subset as {[key: string]: boolean};

    for (const element of whole) {
        if (!subsetMap[element]) {
            result.push(element);
        }
    }

    return result;
}

function is_getter_node(block: FlowGraphNode): boolean {
    if (isAtomicFlowBlockData(block.data)){
        const data = block.data as AtomicFlowBlockData;

        if (data.value.options.type !== 'operation') {
            // Getter or trigger
            return true;
        }
        return false;
    }
    else if (isDirectValueBlockData(block.data)){
        return true;
    }
    else if (isEnumDirectValueBlockData(block.data)){
        return true;
    }
}


function is_trigger_node(graph: FlowGraph, block_id: string, conn_index: EdgeIndex, rev_conn_index: EdgeIndex): boolean {
    const block = graph.nodes[block_id];
    if (isAtomicFlowBlockData(block.data) || isUiFlowBlockData(block.data)){
        const data = block.data;

        const inputs = data.value.options.inputs || [];

        // If it has any pulse input, it's not a source block
        if (inputs.filter(v => is_pulse(v)).length > 0) {
            return false;
        }

        // If it has a getter input, it's not a source block (the getter is)
        let has_block_inputs = false;
        for (const conn of rev_conn_index[block_id] || []) {
            const orig = graph.nodes[conn.from.id];
            if (orig.data.type === AtomicFlowBlock.GetBlockType()) {
                has_block_inputs = true;
                break;
            }
        }

        if (has_block_inputs) {
            return false;
        }

        // If it's a getter check that it gets derived into a trigger
        if (data.value.options.type === 'getter') {
            const find_triggers_downstream_controller = ((_node_id: string, node: FlowGraphNode, _: string[]) => {
                if (isAtomicFlowBlockData(node.data)) {
                    const a_node = node.data;

                    if (a_node.value.options.type === 'getter') {
                        return 'continue'; // This path might be valid, continue checking downstream
                    }
                    else if (a_node.value.options.type === 'operation') {
                        return 'stop'; // This path is not valid
                    }
                    else if (a_node.value.options.type === 'trigger') {
                        return 'capture'; // This is a valid path
                    }
                }
                else if (isUiFlowBlockData(node.data)) {
                    const hasSignalInput = !!((node.data.value.options.inputs || []).find(inp => is_pulse(inp)));
                    const hasSignalOutput = !!((node.data.value.options.outputs || []).find(outp => is_pulse(outp)));

                    if (!hasSignalInput && hasSignalOutput) {
                        // Trigger block, like a button
                        return 'capture'
                    }

                    if (!hasSignalInput && !hasSignalOutput) {
                        // Probably a sink, like a display.
                        //
                        // There are not any cases where a UI block
                        // generates data that doesn't have a trigger-like
                        // function.
                        return 'capture';
                    }

                    if (hasSignalInput) {
                        // Operation, like a counter.
                        // The cases where this should be used for a UI block are not clear.
                        return 'stop';
                    }
                }
                else {
                    throw new Error(`Unexpected: Direct value block should not have an input`);
                }

                throw new Error(`Unexpected signal properties: ${JSON.stringify(node.data)} | ${isUiFlowBlockData(node.data)}`)

            });

            if (!scan_downstream(graph, block_id, conn_index, find_triggers_downstream_controller)) {
                return false; // Ignore this entry if id doesn't derive into a trigger
            }
        }

        return true;
    }
    else {
        return false;
    }
}

export function get_unreachable(graph: FlowGraph): string[] {
    const reached = build_index(get_source_signals(graph));

    let remaining_connections: FlowGraphEdge[] = [].concat(graph.edges);
    let empty_pass = false;

    // Propagate signals forward to check which operation blocks are reachable
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

    // Propagate remaining signals back to see which getters and value nodes are left unused
    do {
        empty_pass = true;

        const skipped: FlowGraphEdge[] = [];
        for (const conn of remaining_connections) {
            if (reached[conn.to.id]) {
                // Connection activated
                if (is_getter_node(graph.nodes[conn.from.id])) {
                    empty_pass = false;
                    reached[conn.from.id] = true;
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

export function get_source_signals(graph: FlowGraph): string[] {
    const signals = [];

    const conn_index = index_connections(graph);
    const rev_conn_index = reverse_index_connections(graph);

    for (const block_id of Object.keys(graph.nodes)) {
        if (is_trigger_node(graph, block_id, conn_index, rev_conn_index)) {
            signals.push(block_id);
        }
    }


    return signals;
}

function has_pulse_output(block: FlowGraphNode): boolean {
    if (isAtomicFlowBlockData(block.data)){
        const outputs = block.data.value.options.outputs || [];

        return outputs.filter(v => is_pulse(v)).length > 0;
    }
    else if (isDirectValueBlockData(block.data)){
        const data = block.data as DirectValueFlowBlockData;

        return is_pulse(data.value);
    }
    else if (isEnumDirectValueBlockData(block.data)){
        return false;
    }
    else if (isUiFlowBlockData(block.data)){
        const outputs = block.data.value.options.outputs || [];

        return outputs.filter(v => is_pulse(v)).length > 0;
    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

function has_pulse_input(block: FlowGraphNode): boolean {
    if (isAtomicFlowBlockData(block.data)){
        const data = block.data as AtomicFlowBlockData;

        const inputs = data.value.options.inputs || [];

        return inputs.filter(v => is_pulse(v)).length > 0;
    }
    else if (isDirectValueBlockData(block.data)){
        return false;
    }
    else if (isEnumDirectValueBlockData(block.data)){
        return false;
    }
    else if (isUiFlowBlockData(block.data)){
        const inputs = block.data.value.options.inputs || [];

        return inputs.filter(v => is_pulse(v)).length > 0;
    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

function is_node_conversor_streaming_to_step(node: FlowGraphNode): boolean {
    if (has_pulse_output(node) && !has_pulse_input(node)) {
        return true;
    }

    if (isUiFlowBlockData(node.data)) {
        return true;
    }

    return false;
}

export function get_conversions_to_stepped(graph: FlowGraph, source_block_id: string): string[] {
    const results: {[key: string]: boolean} = {};
    const reached = build_index([source_block_id]);

    let remaining_connections: FlowGraphEdge[] = [].concat(graph.edges);
    let empty_pass = false;

    // If the source node feeds into a non getter, it should also be considered as a conversor
    const source_block = graph.nodes[source_block_id];
    if (is_node_conversor_streaming_to_step(source_block)) {
        for (const conn of remaining_connections) {
            if (conn.from.id === source_block_id) {
                const target = graph.nodes[conn.to.id];
                if (has_pulse_input(target)) {
                    results[source_block_id] = true;
                    break;
                }
            }
        }
    }

    do {
        empty_pass = true;

        const skipped: FlowGraphEdge[] = [];
        for (const conn of remaining_connections) {
            if (reached[conn.from.id]) {
                const node = graph.nodes[conn.to.id];
                if (is_node_conversor_streaming_to_step(node)) {
                    // Conversor to step
                    results[conn.to.id] = true;
                }
                else {
                    // Part of the streaming flow
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

    return Object.keys(results);
}

export function get_pulse_continuations(graph: FlowGraph, source_id: string): FlowGraphEdge[][] {
    const outputs: {[key: string]: FlowGraphEdge}[] = [];

    const block = graph.nodes[source_id];

    for (const conn of graph.edges) {
        if (conn.from.id === source_id) {
            if (is_pulse_output(block, conn.from.output_index)) {
                if (!outputs[conn.from.output_index]) {
                    outputs[conn.from.output_index] = {};
                }
                outputs[conn.from.output_index][conn.to.input_index + '_' + conn.to.id] = conn;
            }
        }
    }

    return outputs.map(conn_set => Object.values(conn_set));
}

export interface BlockTreeArgument {
    tree: BlockTree, output_index: number
};

export interface BlockTree {
    block_id: string,
    arguments: BlockTreeArgument[],
};

interface BlockTreeOutputValue {
    block: BlockTree;
    output_index: number;
};

export interface SteppedBlockTreeBlock extends BlockTree  {
    contents: SteppedBlockTree[],
};

export interface VirtualSteppedBlock {
    block_id: string,
    type: string,
    contents?: (SteppedBlockTree | SteppedBlockTree[])[],
    arguments?: BlockTreeArgument[],
}

export interface SteppedBlockTreeJump extends VirtualSteppedBlock {
    block_id: string,
    type: 'jump_to_block';
}

export interface SteppedBlockTreeFork extends VirtualSteppedBlock {
    block_id: string,
    type: 'op_fork_execution',
    contents: (SteppedBlockTree | SteppedBlockTree[])[],
}

export type SteppedBlockTree = SteppedBlockTreeBlock | VirtualSteppedBlock;

export function get_tree_with_ends(graph: FlowGraph, top: string, bottom: string): BlockTree {
    const args: FlowGraphEdge[] = [];

    for (const conn of graph.edges) {
        if (conn.to.id === bottom) {
            if (args[conn.to.input_index] !== undefined) {
                throw new Error("Multiple inputs on single port");
            }

            args[conn.to.input_index] = conn;
        }
    }

    return {
        block_id: bottom,
        arguments: args.map(conn => {
            if (conn) {
                return {
                    tree: get_tree_with_ends(graph, top, conn.from.id),
                    output_index: conn.from.output_index,
                };
            }
            else {
                return null;
            }
        })
    };
}

export function get_filters(graph: FlowGraph, source_block_id: string): BlockTree[] {
    const conversions = get_conversions_to_stepped(graph, source_block_id);

    return conversions.map((bottom_id) => {
        if (bottom_id === source_block_id) {
            // Although the source performs the conversion, there's no filter.
            return null;
        }
        return get_tree_with_ends(graph, source_block_id, bottom_id)
    });
}

export function get_stepped_block_arguments(graph: FlowGraph, block_id: string,
                                            source_id: string,
                                            conn_index: EdgeIndex,
                                            rev_conn_index: EdgeIndex,
                                           ): BlockTreeArgument[] {
    const args: BlockTreeArgument[] = [];

    let pulse_offset = 0;
    let pulse_ports: {[key: string]: boolean} = {};

    const arg_conns: IndexedFlowGraphEdge[][] = [];

    for (const conn of rev_conn_index[block_id] || []) {
        if (is_pulse_output(graph.nodes[conn.from.id], conn.from.output_index)) {
            pulse_ports[conn.to.input_index] = true;
            pulse_offset = Math.max(pulse_offset, conn.to.input_index + 1);
        }
        else {
            const idx = conn.to.input_index;
            pulse_ports[idx] = false;

            if (!arg_conns[idx]) {
                arg_conns[idx] = [];
            }
            arg_conns[idx].push(conn);
        }
    }

    for (let idx = 0; idx < arg_conns.length;idx++) {

        if (!arg_conns[idx]) {
            continue;
        }

        let conn = arg_conns[idx][0];
        if (arg_conns[idx].length > 1) {
            // If multiple connections, take the one from source_id, if not found, that's an error.
            conn = arg_conns[idx].find(c => c.from.id === source_id);

            if (!conn) {
                throw new Error("Multiple inputs on single port."
                                + " This is only allowed if the inputs correspond to the different triggers, but it's not the case here.");
            }
        }

        args[conn.to.input_index] = {
            tree: {
                block_id: conn.from.id,
                arguments: get_stepped_block_arguments(graph, conn.from.id, source_id, conn_index, rev_conn_index),
            },
            output_index: conn.from.output_index,
        };
    }

    // Validate that all pulse's are grouped
    for (let i = 0; i < pulse_offset; i++) {
        if (pulse_ports[i]) {
            args.shift();
        }
        else {
            throw new Error(`Non-pulse input before a pulse one on block_id:${block_id} (Port: ${pulse_offset}. Block: ${JSON.stringify(graph.nodes[block_id].data)})`);
        }
    }

    return args;
}

function get_stepped_ast_continuation(graph: FlowGraph,
                                      continuation: FlowGraphEdge,
                                      source_id: string,
                                      conn_index: EdgeIndex,
                                      rev_conn_index: EdgeIndex,
                                      ast: SteppedBlockTree[],
                                      reached: {[key: string]: boolean}) {

    const block_id = continuation.to.id;

    if (reached[block_id]) {
        // Create new jump-to block here

        ast.push({
            block_id: block_id,
            type: JUMP_TO_BLOCK_OPERATION
        });
    }
    else {
        ast.push({
            block_id: block_id,
            arguments: get_stepped_block_arguments(graph, block_id, source_id, conn_index, rev_conn_index),
            contents: [],
        });

        reached[block_id] = true;
        get_stepped_ast_branch(graph, block_id, ast, reached);
    }
}

function cut_on_block_id(ast: SteppedBlockTree[], block_id: string): [SteppedBlockTree[], SteppedBlockTree[]] {
    for (let cut_idx = 0;cut_idx < ast.length; cut_idx++) {
        if (ast[cut_idx].block_id === block_id) {
            return [
                ast.slice(0, cut_idx),
                ast.slice(cut_idx),
            ];
        }
    }

    // If the block is not found, the merge point is not in scope
    return [
        ast,
        [],
    ];
}

function find_common_merge(asts: SteppedBlockTree[][], options: { prune_not_finishing: boolean }): { asts: SteppedBlockTree[][], common_suffix: SteppedBlockTree[] } {
    const findings: { [key: string]: number[]} = {};
    const common_blocks: [string, number][] = [];

    if (asts.length === 0) {
        return null;
    }

    let not_finishing = [];
    if (options && options.prune_not_finishing) {
        for (let idx = 0; idx < asts.length; idx++) {
            const ast = asts[idx];

            for (let op_idx = 0; op_idx < ast.length; op_idx++) {
                const op = ast[op_idx];

                if ((op as VirtualSteppedBlock).type) {
                    const fun = (op as VirtualSteppedBlock).type;

                    if (fun === JUMP_TO_BLOCK_OPERATION || fun === JUMP_TO_POSITION_OPERATION) {
                        not_finishing.push(idx);
                        break;
                    }
                }
            }
        }
        if (not_finishing.length === asts.length) {
            not_finishing = [];
        }
    }


    for (let idx = 0; idx < asts.length; idx++) {
        const ast = asts[idx];

        for (let op_idx = 0; op_idx < ast.length; op_idx++) {
            const op = ast[op_idx];

            const block_id = op.block_id;
            if (!findings[block_id]) {
                findings[block_id] = [];
            }

            findings[block_id].push(op_idx);
            if ((findings[block_id].length + not_finishing.length) === asts.length) {
                common_blocks.push([block_id, findings[block_id].reduce((a, b) => a + b, 0)]);
            }
            else if (findings[block_id].length > asts.length) {
                throw new Error(`Duplicated block (id=${block_id}) found on ast`);
            }
        }
    }

    if (common_blocks.length === 0) {
        return null;
    }

    const sorted_ascending = common_blocks.sort((a, b) => a[1] - b[1]);
    const first_cut_id = sorted_ascending[0][0];

    let common_suffix = null;
    const differences = [];
    for (let idx = 0; idx < asts.length; idx++) {
        if (not_finishing.indexOf(idx) >= 0) {
            differences.push(asts[idx]);
        }
        else {
            const [unique_ast, suffix] = cut_on_block_id(asts[idx], first_cut_id);
            if (common_suffix === null) {
                common_suffix = suffix;
            }
            differences.push(unique_ast);
        }
    }

    if (common_suffix === null) {
        throw new Error('Unexpected: No suffix, but it should have.');
    }

    return {
        asts: differences,
        common_suffix: common_suffix,
    }
}

function find_common_merge_groups_ast(asts: SteppedBlockTree[][],
                                      options: { prune_not_finishing: boolean, remove_empty: boolean }
                                     ): SteppedBlockTree[][] {

    const findings: { [key: string]: [number, number][]} = {};
    const common_blocks: {[key:string]: [number[], number]} = {};
    const grouped: {[key: string]: boolean} = {};

    if (asts.length === 0) {
        return null;
    }

    for (let idx = 0; idx < asts.length; idx++) {
        const ast = asts[idx];

        // Remove empty ASTs if so requested (used on Fork() blocks).
        if ((ast.length === 0) && options.remove_empty) {
            asts.splice(idx, 1);
            idx--;
            continue;
        }

        const found_blocks: {[key: string]: boolean} = {};

        for (let op_idx = 0; op_idx < ast.length; op_idx++) {
            const op = ast[op_idx];

            const block_id = op.block_id;
            // Stop if already found by this same column
            // This means a there's a loop inside the fork
            if (found_blocks[block_id]) {
                break;
            }
            found_blocks[block_id] = true;

            if (!findings[block_id]) {
                findings[block_id] = [];
            }

            findings[block_id].push([idx, op_idx]);
            const block_findings = findings[block_id];

            if (block_findings.length > 1) {
                grouped[idx] = true;
                common_blocks[block_id] = [block_findings.map(v => v[0]),
                                           block_findings.reduce((acc, val) => acc + val[1], 0)];
            }
            if (block_findings.length === 2) {
                // Add also the first on the list
                grouped[block_findings[0][0]] = true;
            }
        }
    }

    if (Object.keys(grouped).length === 0) {
        return null; // No groups found
    }

    if (Object.keys(common_blocks).length === 0) {
        throw new Error('This should not happen. Groups found but no common_blocks');
    }

    const groups: number[][] = [];
    const group_index: {[key: string]: boolean} = {};
    for (const block of Object.values(common_blocks)) {
        if (!group_index[block[0].toString()]) {
            group_index[block[0].toString()] = true;
            groups.push(block[0]);
        }
    }

    groups.sort((x, y) => y.length - x.length); // Descending length

    const group_asts: { asts: SteppedBlockTree[][], common_suffix: SteppedBlockTree[] }[] = [];
    const in_previous_group: {[key: number]: [number, number]} = {};

    // Build group tree
    for (let group_idx = 0; group_idx < groups.length; group_idx++) {
        const column_asts = [];
        const inserts = [];
        const deletes = [] ;
        let ast_idx = -1;
        const inserted_asts: {[key: string]: boolean} = {};
        for (const column of groups[group_idx]) {
            ast_idx++;

            if (in_previous_group[column] !== undefined) {
                const prev_group = in_previous_group[column];
                const splitted_ast = group_asts[prev_group[0]];

                if (!inserted_asts[prev_group[0]]) {
                    inserts.push(prev_group[0]);
                }
                inserted_asts[prev_group[0]] = true;

                column_asts.push(splitted_ast.asts[prev_group[1]]);
                deletes.push({ group: prev_group[0], position: prev_group[1] })

                in_previous_group[column] = [group_idx, ast_idx];
            }
            else {
                column_asts.push(asts[column]);
                in_previous_group[column] = [group_idx, ast_idx];
            }
        }

        const merged_ast = find_common_merge(column_asts, options);
        if (!merged_ast){
            throw new Error(`Error merging asts (idx:${groups[group_idx]})`)
        }

        for (const _delete of deletes) {
            delete group_asts[_delete.group].asts[_delete.position];
        }

        if (inserts.length === 0) {
            group_asts.push(merged_ast);
        }
        else {
            for (const insert of inserts) {
                group_asts[insert].asts.push(merged_ast as any);
            }
        }
    }

    const grouped_ast_tree: (SteppedBlockTree[] | { asts: SteppedBlockTree[][], common_suffix: SteppedBlockTree[] })[] = group_asts;

    for (let idx = 0; idx < asts.length; idx++) {
        if (!grouped[idx]) {
            grouped_ast_tree.push(asts[idx]);
        }
    }

    // Clean grouped AST tree
    const result: SteppedBlockTree[][] = [];

    function compile_group(e: (SteppedBlockTree[] | { asts: SteppedBlockTree[][],
                                                      common_suffix: SteppedBlockTree[] })): SteppedBlockTree[] {
        // Just copy non-grouped ASTs
        if (!(e as any).common_suffix) {
            return e as SteppedBlockTree[];
        }

        const group = e as { asts: SteppedBlockTree[][], common_suffix: SteppedBlockTree[] };

        const fork_ref = uuidv4();

        let commons: SteppedBlockTree[] = [];
        for (const common of group.common_suffix) {
            if (common !== undefined) {
                commons = commons.concat(compile_group(common as any));
            }
        }

        const contents: SteppedBlockTree[][] = [];
        for (const content of group.asts) {
            if (content !== undefined) {
                contents.push(compile_group(content));
            }
        }

        const fork_block: SteppedBlockTreeFork = {
            block_id: fork_ref,
            type: FORK_OPERATION,
            arguments: [],
            contents: contents,
        };
        return (([fork_block] as SteppedBlockTree[]).concat(commons));

    }

    for (const group of grouped_ast_tree) {
        if (group !== undefined) {
            result.push(compile_group(group));
        }
    }

    return result;
}

function get_stepped_ast_branch(graph: FlowGraph, source_id: string, ast: SteppedBlockTree[], reached: {[key: string]: boolean}) {
    let continuations = get_pulse_continuations(graph, source_id);

    const conn_index = index_connections(graph);
    const rev_conn_index = reverse_index_connections(graph);

    if (continuations.length > 1) {
        let contents: any /*: (SteppedBlockTree | SteppedBlockTree[])[]*/ = [];

        for (const cont of continuations) {
            const subast: SteppedBlockTree[] = [];

            if (!cont || cont.length === 0) {
                contents.push([]);
            }
            else if (cont.length > 1) {
                throw new Error(`There should be one and only one pulse per output`)
            }
            else {
                const subreached = Object.assign({}, reached)
                get_stepped_ast_continuation(graph, cont[0], source_id, conn_index, rev_conn_index, subast, subreached);
                contents.push(subast);
            }
        }

        // Pruning has to be done on if-else constructions to properly handle loops
        let prune_not_finishing = false;
        const node = graph.nodes[source_id];
        if (node.data.type === AtomicFlowBlock.GetBlockType()) {
            const fun = (node.data as AtomicFlowBlockData).value.options.block_function;
            const functions_requiring_prune = ['control_if_else'];

            prune_not_finishing = functions_requiring_prune.indexOf(fun) >= 0;
        }

        let common_suffix: SteppedBlockTree[] = [];
        const source_node = graph.nodes[source_id];
        if (source_node.data.type === AtomicFlowBlock.GetBlockType() &&
            (source_node.data as AtomicFlowBlockData).value.options.block_function === FORK_OPERATION) {

            const merged_groups = find_common_merge_groups_ast(contents, { prune_not_finishing: false, remove_empty: true });
            if (merged_groups) {
                if (merged_groups.length === 1){
                    // There's a top level fork, just replace it
                    const fork_block = merged_groups[0][0];
                    if ((fork_block as VirtualSteppedBlock).type !== FORK_OPERATION) {
                        throw new Error(`Unexpected: Expecting first block of common merge to be ${FORK_OPERATION},`
                                        + ` found ${(fork_block as VirtualSteppedBlock).type}`);
                    }

                    contents = fork_block.contents;
                    common_suffix = merged_groups[0].slice(1);
                }
                else {
                    contents = merged_groups;
                    common_suffix = [];
                }
            }
        }
        else if (source_node.data.type === AtomicFlowBlock.GetBlockType() &&
            (source_node.data as AtomicFlowBlockData).value.options.block_function === REPEAT_OPERATION) {
            // Set everything in place for REPEAT <times> blocks

            common_suffix = contents[2];
            // contents[1] is ignored as it's not a pulse output
            contents = contents[0];
        }
        else {
            const re_merge_data = find_common_merge(contents, { prune_not_finishing });
            if (re_merge_data) {
                contents = re_merge_data.asts;
                common_suffix = re_merge_data.common_suffix;
            }
        }


        ast[ast.length - 1].contents = contents; // Update parent block contents
        for (const op of common_suffix) {
            ast.push(op);
        }
    }

    if (continuations.length == 1) {
        if (continuations[0].length == 1) {
            const block = graph.nodes[source_id];
            const is_atomic_block = isAtomicFlowBlockData(block.data);
            let atom_data: AtomicFlowBlockData = null;
            let func_name: string = null;
            if (is_atomic_block) {
                atom_data = block.data as AtomicFlowBlockData;
                func_name = atom_data.value.options.block_function;
            }

            if (is_atomic_block && func_name === 'control_if_else') {
                // IF statement with only one output
                const contents: SteppedBlockTree[] = [];
                get_stepped_ast_continuation(graph, continuations[0][0], source_id, conn_index, rev_conn_index, contents, reached);

                ast[ast.length - 1].contents = [contents, []]; // Update parent block contents
            }
            else {
                get_stepped_ast_continuation(graph, continuations[0][0], source_id, conn_index, rev_conn_index, ast, reached);
            }
        }
        else {
            throw new Error(`Multiple outputs pulses from the same port`);
        }
    }
    else if (continuations.length == 0) {
        // Empty AST
        // Nothing to do
    }
}

export function get_stepped_ast(graph: FlowGraph, source_id: string): SteppedBlockTree[] {
    const result: SteppedBlockTree[] = [];

    get_stepped_ast_branch(graph, source_id, result, {source_id: true})

    return result;
}

function compile_contents(graph: FlowGraph, contents: SteppedBlockTree[]): CompiledBlock[] {
    let latest = null;
    const results = [];

    for (const block of contents) {
        const compiled = compile_block(graph, block.block_id, block.arguments, block.contents as SteppedBlockTree[],
                                       { inside_args: false, orig_tree: block },
                                       { before: latest }
                                      )
        if (compiled !== null) {
            results.push(compiled);
            latest = compiled;
        }
    }

    return results;
}


function already_used_in_past(graph: FlowGraph, arg_id: string,
                              parent: string,
                              reference_block: string): boolean {

    // TODO: Don't re-index every time this function is called
    const rev_conn_index = reverse_index_connections(graph);

    let has_pulse_input = false;
    const node = graph.nodes[reference_block];
    if (node.data.type === AtomicFlowBlock.GetBlockType()) {
        const ref_node = node.data as AtomicFlowBlockData;

        if (ref_node.value.options.type === 'operation') {
            has_pulse_input = true;
        }
        else if (ref_node.value.options.type === 'trigger') {
            // Trigger have no pulse inputs, but consider the signals to be cached
            // TODO: Don't find source blocks each time this function is called
            const sources = get_source_signals(graph);
            if (sources.indexOf(arg_id) >= 0) {
                return true;
            }
        }
    }

    for (const conn of rev_conn_index[reference_block]) {
        const is_pulse_connection = is_pulse_output(graph.nodes[conn.from.id], conn.from.output_index);
        if (has_pulse_input && !is_pulse_connection) {
            // Skip first level non-pulse outputs if the reference block has any
            // pulse input
            continue;
        }

        // Consider direct pulse outputs (mostly from triggers)
        if (is_pulse_connection) {
            if (conn.from.id === arg_id) {
                return true;
            }
        }

        const result = (scan_upstream(graph, conn.from.id, rev_conn_index,
                                      (found_id: string, _, path: string[]) => {
                                          if (found_id === arg_id) {
                                              if (path[path.length - 2] === parent) {
                                                  // Ignore this match and branch if it's just above the parent
                                                  return 'stop';
                                              }
                                              return 'capture';
                                          }
                                          else {
                                              return 'continue';
                                          }
                                      }));
        if (result) {
            return true;
        }
    }

    return false;
}

function compile_arg(graph: FlowGraph, arg: BlockTreeArgument, parent: string, orig: string, arg_index: number, before: CompiledBlock | null): CompiledBlockArg {
    const block = graph.nodes[arg.tree.block_id];

    if (isAtomicFlowBlockData(block.data)){
        if (block.data.value.options.block_function === 'op_on_block_run') {
            const values = arg.tree.arguments.map(arg => graph.nodes[arg.tree.block_id].data);
            if ((values.length != 2)
                || !isDirectValueBlockData(values[0])
                || !isDirectValueBlockData(values[1])) {

                    throw new Error(`Expected 2 direct values for op_on_block_run, found: ${JSON.stringify(values)}`)
            }

            return {
                type: 'block',
                value: [
                    {
                        type: 'flow_last_value',
                        contents: [],
                        args: [
                            {
                                type: 'constant',
                                value: values[0].value.value,
                            },
                            {
                                type: 'constant',
                                value: values[1].value.value,
                            },
                        ],
                    }
                ]
            }
        }
        if (already_used_in_past(graph, arg.tree.block_id, parent, orig)) {
            return {
                type: 'block',
                value: [
                    {
                        type: 'flow_last_value',
                        contents: [],
                        args: [
                            {
                                type: 'constant',
                                value: arg.tree.block_id,
                            },
                            {
                                type: 'constant',
                                value: arg.output_index,
                            }
                        ]
                    }
                ]
            }
        }
        else{
            return {
                type: 'block',
                value: [ compile_block(graph, arg.tree.block_id, arg.tree.arguments, [],
                                       { inside_args: true, orig_tree: null, arg_index: arg_index },
                                       { before: before, exec_orig: orig },
                                      ) ]
            }
        }
    }
    else if (isDirectValueBlockData(block.data)){
        return {
            type: 'constant',
            value: block.data.value.value,
        };
    }
    else if (isEnumDirectValueBlockData(block.data)){
        let value = block.data.value.value_id;

        const definition = block.data.value.options.definition;
        if (definition && definition.type === 'enum_sequence') {
            value = EnumDirectValue.cleanSequenceValue(value);
        }

        return {
            type: 'constant',
            value: value,
        };
    }
    else if (isUiFlowBlockData(block.data)) {
        return {
            type: 'block',
            value: [
                {
                    type: 'data_ui_block_value',
                    args: [ {
                        type: 'constant',
                        value: arg.tree.block_id
                    } ],
                    contents: [],
                }
            ],
        };
    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

function get_latest_blocks_on_each_ast_level(block: CompiledBlock): CompiledBlock[] {
    const results: CompiledBlock[] = [];
    const levels = [block.contents];

    while (levels.length > 0) {
        const next = levels.pop();
        if (next.length === 0) {
            continue;
        }

        if ((next[0] as CompiledBlock).type) {
            // Are compiled blocks, so take the last and add to level
            const latest = next[next.length - 1];
            results.push(latest as CompiledBlock);
            levels.push(latest.contents)
        }
        else {
            for (const content of next){
                levels.push(content.contents);
            }
        }
    }

    return results;
}

function compile_block(graph: FlowGraph,
                       block_id: string,
                       args: BlockTreeArgument[],
                       contents: SteppedBlockTree[][] | SteppedBlockTree[],
                       flags: { inside_args: boolean, orig_tree: SteppedBlockTree, arg_index?: number },
                       relatives: { before: CompiledBlock, exec_orig?: string }): CompiledBlock {

    if (flags.orig_tree && (flags.orig_tree as VirtualSteppedBlock).type) {
        const vblock = flags.orig_tree as VirtualSteppedBlock;

        if (vblock.type === 'jump_to_block') {
            return {
                type: JUMP_TO_BLOCK_OPERATION,
                args: [{ type: 'constant', value: block_id }],
                contents: [],
            };
        }
        else if (vblock.type === 'op_fork_execution') {
            return {
                type: FORK_OPERATION,
                args: [],
                contents: (contents as SteppedBlockTree[][]).map(c => { return { contents: compile_contents(graph, c)} })
            };
        }
        else {
            throw new Error(`Unknown virtual block (type: ${vblock.type})`);
        }
    }

    const block = graph.nodes[block_id];

    if (!block) {
        if (flags) {
            throw new Error(`Block not found (id: ${block_id}, inside_args: ${flags.inside_args})\nValue: ${JSON.stringify(flags.orig_tree)}`)
        }
        else {
            throw new Error(`Block not found (id: ${block_id}, no flags)`)
        }
    }

    if (isAtomicFlowBlockData(block.data)){
        const data = block.data;

        let compiled_contents: (CompiledBlock | ContentBlock)[] = [];
        if (contents && contents.length) {
            if (flags.inside_args) {
                throw new Error("Found block with contents inside args");
            }

            if (contents.length > 0) {
                if ((contents as SteppedBlockTree[][])[0] instanceof Array) {
                    compiled_contents = (contents as SteppedBlockTree[][]).map(v => {
                        return {
                            contents: compile_contents(graph, v)
                        }
                    })
                }
                else {
                    compiled_contents = compile_contents(graph, contents as SteppedBlockTree[]);
                }
            }
        }

        let block_type = null;
        let compiled_args: CompiledBlockArgs = args.map(v => compile_arg(graph, v,
                                                                         block_id,
                                                                         relatives.exec_orig ? relatives.exec_orig : block_id,
                                                                         v.output_index,
                                                                         relatives.before));
        const block_fun = data.value.options.block_function;
        const slot_args: CompiledBlockArgList = [];

        const slots = data.value.slots;

        if (slots) {
            for (const slot of Object.keys(slots)){
                slot_args.push({ type: slot as any, value: slots[slot]})
            }
        }

        // Prepend slots (sorted) to args
        compiled_args = slot_args.concat(compiled_args);

        // Only wait for time monitors if the same block has not been used before on the same flow
        if (TIME_TRIGGERS.indexOf(block_fun) >= 0) {
            const key = block_fun === 'flow_utc_time' ? 'utc_time' : 'utc_date';

            let alreadyRun: boolean | null = null;
            if (flags.inside_args) {
                if (relatives.before && relatives.before.id === block_id) {
                    alreadyRun = true;
                }
                else if (relatives.before) {

                    // TODO: Don't re-index every time this function is called
                    const rev_conn_index = reverse_index_connections(graph);

                    const foundBefore = scan_upstream(graph, relatives.before.id, rev_conn_index,
                                                      (node_id: string, _node: FlowGraphNode) => {
                                                          if (node_id === block_id) {
                                                              return 'capture';
                                                          }
                                                          else {
                                                              return 'continue';
                                                          }
                                                      });

                    if (foundBefore) {
                        alreadyRun = true;
                    }
                }
            }

            // Is not an arg (so, it's a signal) or it's a new block
            if (!flags.inside_args) {
                block_type = "wait_for_monitor";
                compiled_args = {
                    monitor_id: {
                        from_service: TIME_MONITOR_ID,
                    },
                    key: key,
                    monitor_expected_value: 'any_value',
                };
            }
            else if (!alreadyRun) {
                block_type = "wait_for_monitor";
                compiled_args = {
                    monitor_id: {
                        from_service: TIME_MONITOR_ID,
                    },
                    key: key,
                    monitor_expected_value: 'any_value',
                    monitored_value: flags.arg_index,
                };
            }
            else {
                return {
                    type: "flow_last_value",
                    args: [
                        {
                            type: 'constant',
                            value: block_id,
                        },
                        {
                            type: 'constant',
                            value: flags.arg_index,
                        }
                    ],
                    contents: [],
                };
            }
        }
        else if (block_fun.startsWith('services.')) {
            if (data.value.options.type === 'trigger') {
                // Ignore
                block_type = block_fun;

                if (flags.inside_args) {
                    return {
                        type: "flow_last_value",
                        args: [
                            {
                                type: 'constant',
                                value: block_id,
                            },
                            {
                                type: 'constant',
                                value: flags.arg_index,
                            }
                        ],
                        contents: [],
                    };
                }
                else {
                    const listenerArgs: {key: string, subkey?: any, monitor_expected_value?: any} = {
                        key: block_fun.split('.').reverse()[0],
                    };

                    if (data.value.options.key) {
                        listenerArgs.key = data.value.options.key;
                    }

                    const subkey = data.value.options.subkey;
                    if (subkey) {
                        listenerArgs.subkey = compiled_args[subkey.index - 1];
                        compiled_args.splice(subkey.index - 1, 1);
                    }

                    if (compiled_args.length > 0) {
                        listenerArgs.monitor_expected_value = compiled_args[0];
                    }

                    compiled_args = (listenerArgs as any);
                }
            }
            else {
                block_type = "command_call_service";
                const [, bridge_id, call_name] = block_fun.split('.');
                compiled_args = {
                    service_id: bridge_id,
                    service_action: call_name,
                    service_call_values: compiled_args,
                };
            }
        }
        else if (block_fun === 'trigger_when_all_true') {
            block_type = "control_if_else";

            // Tie arguments with an *and* operation
            compiled_args = [{
                type: 'block',
                value: [{
                    type: "operator_and",
                    contents: [],
                    args: compiled_args,
                }],
            }];
        }
        else if (block_fun === 'trigger_when_first_completed') {
            // Natural exit of an IF.
            // This block is structural and MUST not appear on a compiled AST.
            if (relatives.before && ['control_if_else', FORK_OPERATION].indexOf(relatives.before.type) < 0 ) {
                throw new Error("Expected block 'trigger_when_first_completed' 'control_if_else' or 'op_fork_execution'");
            }

            if (relatives.before) {
                const all_before = get_latest_blocks_on_each_ast_level(relatives.before).concat([relatives.before]);

                for (const block of all_before) {
                    if (block.type === FORK_OPERATION) {
                        if (!block.args) {
                            block.args = [{
                                type: 'constant',
                                value: 'exit-when-first-completed'
                            }];
                        }
                        else if (Array.isArray(block.args)) {
                            if (block.args
                                .filter(a => a.type === 'constant'
                                    && (a.value === 'exit-when-first-completed'
                                        || a.value === 'exit-when-all-completed'))
                                .length === 0) {

                                block.args.push({
                                    type: 'constant',
                                    value: 'exit-when-first-completed'
                                });
                            }
                        }
                    }
                }
            }

            return null;
        }
        else if (block_fun === 'trigger_when_all_completed') {
            // Natural exit of a Fork
            // This block is structural and MUST not appear on a compiled AST.
            if (relatives.before && relatives.before.type === FORK_OPERATION ) {
                // Nothing to add
                const all_before = get_latest_blocks_on_each_ast_level(relatives.before).concat([relatives.before]);

                for (const block of all_before) {
                    if (block.type === FORK_OPERATION) {
                        if (!block.args) {
                            block.args = [{
                                type: 'constant',
                                value: 'exit-when-all-completed'
                            }];
                        }
                        else if (Array.isArray(block.args)) {
                            if (block.args
                                .filter(a => a.type === 'constant'
                                    && (a.value === 'exit-when-first-completed'
                                        || a.value === 'exit-when-all-completed'))
                                .length === 0) {

                                block.args.push({
                                    type: 'constant',
                                    value: 'exit-when-all-completed'
                                });
                            }
                        }
                    }
                }
            }
            else {
                throw new Error("Expected block 'trigger_when_all_completed' after 'op_fork_execution'");
            }
            return null;
        }
        else if (BASE_TOOLBOX_BLOCKS[block_fun]) {
            block_type = block_fun;
        }
        else {
            throw new Error("Unknown block: " + block_fun);
        }

        return {
            id: block_id,
            type: block_type as CompiledBlockType,
            args: compiled_args,
            contents: compiled_contents,
            report_state: block.data.value.report_state,
        };
    }
    else if (isDirectValueBlockData(block.data)){
        const data = block.data;

        if (contents.length > 0) {
            throw new Error("AssertionError: Contents.length > 0 in DirectValue block")
        }

        return {
            type: 'constant',
            value: data.value.value,

            // Not really a compiled block, this would be an argument, but this simplifies things
        } as any as CompiledBlock;
    }
    else if (isEnumDirectValueBlockData(block.data)){
        const data = block.data as EnumDirectValueFlowBlockData;

        if (contents.length > 0) {
            throw new Error("AssertionError: Contents.length > 0 in EnumDirectValue block")
        }

        return {
            type: 'constant',
            value: data.value.value_id,

            // Not really a compiled block, this would be an argument, but this simplifies things
        } as any as CompiledBlock;
    }
    else if (isUiFlowBlockData(block.data)) {

        const compiled_args: CompiledBlockArgs = args.map(v => compile_arg(graph, v,
                                                                           block_id,
                                                                           relatives.exec_orig ? relatives.exec_orig : block_id,
                                                                           v.output_index,
                                                                           relatives.before));

        return {
            id: block_id,
            type: ('services.ui.' + block.data.value.options.id + '.' + block_id) as any,
            args: compiled_args,
            contents: [],
        };

    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

type BlockPositionIndex = {[key: string]: number[]};

function process_jump_index_on_contents(contents: (CompiledBlock | ContentBlock)[],
                                        index: BlockPositionIndex,
                                        prefix: number[]) {

    for (let idx = 0; idx < contents.length; idx++) {
        const op = contents[idx];

        if ((op as CompiledBlock).type === 'jump_point') {
            const block = (op as CompiledBlock);
            index[(block.args as CompiledBlockArgList)[0].value] = prefix.concat([idx])

            // Remove this operation
            contents.splice(idx, 1);
            idx--;

            if (op.contents && op.contents.length) {
                throw new Error('Jump point must not have contents');
            }
        }
        else if ((op as CompiledBlock).id) {
            const block = (op as CompiledBlock);
            index[block.id] = prefix.concat([idx])
        }
        op.contents = process_jump_index_on_contents(op.contents || [], index, prefix.concat([idx]));
    }

    return contents;
}

function process_graph_jump_index(graph: CompiledFlowGraph): BlockPositionIndex {
    const index: BlockPositionIndex = {};
    process_jump_index_on_contents(graph, index, []);

    return index;
}

function link_jumps(contents: (CompiledBlock | ContentBlock)[], positions: BlockPositionIndex) {
    for (const op of contents) {

        if ((op as CompiledBlock).type === JUMP_TO_BLOCK_OPERATION) {
            const block = (op as CompiledBlock);

            const link = (block.args as CompiledBlockArgList)[0].value as string;
            block.type = JUMP_TO_POSITION_OPERATION;
            if (!positions[link]) {
                throw new Error(`Cannot link to "${link}"`)
            }

            (block.args as CompiledBlockArgList)[0].value = positions[link];
        }

        link_jumps(op.contents || [], positions);
    }
}

export function _link_graph(graph: CompiledFlowGraph): CompiledFlowGraph {
    const block_id_index = process_graph_jump_index(graph);

    link_jumps(graph, block_id_index);

    return graph;
}

function get_argument_sources(graph: FlowGraph, block: BlockTree, output_index: number): BlockTreeOutputValue[] {
    const blockArgs = block.arguments.filter( arg => {
        const node = graph.nodes[arg.tree.block_id];

        if (isAtomicFlowBlockData(node.data) || isUiFlowBlockData(node.data)) {
            return true;
        }
        else if (isDirectValueBlockData(node.data) || isEnumDirectValueBlockData(node.data)) {
            return false;
        }
        else {
            throw new Error("Unknown block type: " + node.data.type)
        }
    });

    if (blockArgs.length === 0) {
        return [{block, output_index}];
    }

    // This could be done with a flatMap, but that is fairly recent, so if we
    // can increase the compatibility with a little boilerplate, it's not a bad
    // tradeoff
    let results: BlockTreeOutputValue[] = [];
    for (const arg of blockArgs) {
        results = results.concat(get_argument_sources(graph, arg.tree, arg.output_index));
    }
    return results;
}

function build_signal_from_source(graph: FlowGraph, source: BlockTreeOutputValue): CompiledBlock {
    const source_node = graph.nodes[source.block.block_id];
    if (isAtomicFlowBlockData(source_node.data)) {
        const desc = source_node.data.value.options;
        if (desc.block_function === 'data_variable') {
            return {
                id: source.block.block_id,
                type: "on_data_variable_update",
                args: [
                    {
                        type: "variable",
                        value: source_node.data.value.slots['variable'],
                    }
                ],
                contents: [],
            }
        }
        else if (desc.block_function === 'data_itemoflist') {
            return {
                id: source.block.block_id,
                type: "on_data_variable_update",
                args: [
                    {
                        type: "variable",
                        value: source_node.data.value.slots['list'],
                    }
                ],
                contents: [],
            }
        }
        else if (desc.type === 'operation') {
            return {
                type: "flow_last_value",
                args: [
                    {
                        type: 'constant',
                        value: source.block.block_id,
                    },
                    {
                        type: 'constant',
                        value: source.output_index + '',
                    }
                ]
            }
        }
        else if (desc.block_function === 'op_on_block_run') {
            const compiled_args: CompiledBlockArgs = source.block.arguments.map(v => compile_arg(graph, v,
                                                                                                 source.block.block_id,
                                                                                                 source.block.block_id,
                                                                                                 v.output_index,
                                                                                                 null, // Signal, so nothing before
                                                                                                ));

            return {
                id: source.block.block_id,
                type: "op_on_block_run",
                args: compiled_args,
                contents: [],
            }
        }
        else if (desc.block_function.startsWith('services.')) {
            // Rewrite service calls to custom triggers
            const compiled_args: CompiledBlockArgs = source.block.arguments.map(v => compile_arg(graph, v,
                                                                                                 source.block.block_id,
                                                                                                 source.block.block_id,
                                                                                                 v.output_index,
                                                                                                 null, // Signal, so nothing before
                                                                                                ));

            const listenerArgs: {key: string, subkey?: any, monitor_expected_value?: any} = {
                key: desc.block_function.split('.').reverse()[0],
            };

            if (desc.key) {
                listenerArgs.key = desc.key;
            }

            const subkey = desc.subkey;
            if (subkey) {
                listenerArgs.subkey = compiled_args[subkey.index - 1];
                compiled_args.splice(subkey.index - 1, 1);
            }

            if (compiled_args.length > 0) {
                listenerArgs.monitor_expected_value = compiled_args[0];
            }

            return {
                id: source.block.block_id,
                type: (desc.block_function as any),
                args: (listenerArgs as any),
                contents: [],
            }
        }
        else if (TIME_TRIGGERS.indexOf(desc.block_function) >= 0) {
            const key = desc.block_function === 'flow_utc_time' ? 'utc_time' : 'utc_date';
            return {
                type: "wait_for_monitor",
                args: {
                    monitor_id: {
                        from_service: TIME_MONITOR_ID,
                    },
                    key: key,
                    monitor_expected_value: 'any_value',
                },
                id: source.block.block_id,
            }
        }
        else {
            throw new Error(`Unexpected flow source node: (fun: ${desc.block_function}, type: ${source_node.data.type}, id: ${source.block.block_id})`);
        }
    }
    else if (isUiFlowBlockData(source_node.data)) {
        const compiled_args: CompiledBlockArgs = source.block.arguments.map(v => compile_arg(graph, v,
                                                                                             source.block.block_id,
                                                                                             source.block.block_id,
                                                                                             v.output_index,
                                                                                             null, // Signal, so nothing before
                                                                                            ));

        return {
            id: source.block.block_id,
            type: ('services.ui.' + source_node.data.value.options.id + '.' + source.block.block_id) as any,
            args: compiled_args,
            contents: [],
        };
    }
    else {
        throw new Error(`Unexpected flow source node: (type: ${source_node.data.type}, id: ${source.block.block_id})`);
    }
}

function build_stream_for_source(graph: FlowGraph,
                                 source: BlockTreeOutputValue,
                                 sink: BlockTree,
                                 before: CompiledBlock
                                ): CompiledBlock[] {
    const signal = build_signal_from_source(graph, source);

    return [
        signal,
        compile_block(graph, sink.block_id, sink.arguments, [],
                      { inside_args: false, orig_tree: null },
                      { before: before }),
    ];
}

function build_streaming_flow_to(graph: FlowGraph,
                                 sink: BlockTree,
                                 before: CompiledBlock,
                                ) : CompiledBlock[][] {

    if (sink.arguments.length != 1) {
        throw new Error("Not implemented `build_streaming_flow_to` for argument list length != 1");
    }

    const sources = get_argument_sources(graph, sink, null);
    const programs = sources.map((source: BlockTreeOutputValue) => build_stream_for_source(graph, source, sink, before));

    return programs;
}

export function assemble_flow(graph: FlowGraph,
                              signal_id: string,
                              filter: BlockTree | null,
                              stepped_ast: SteppedBlockTree[]): CompiledFlowGraph[] {

    const conn_index = index_connections(graph);
    const rev_conn_index = reverse_index_connections(graph);

    const signal_ast = compile_block(graph, signal_id, get_stepped_block_arguments(graph, signal_id, null, conn_index, rev_conn_index), [],
                                     { inside_args: false, orig_tree: null },
                                     { before: null });

    let skip_filter = false;
    let compiled_graph: CompiledBlock[] = null;

    if (filter) {
        const filter_node = graph.nodes[filter.block_id];

        if (isAtomicFlowBlockData(filter_node.data)) {
            if (filter_node.data.value.options.block_function === 'trigger_on_signal') {
                skip_filter = true;
            }
        }
        else if (isUiFlowBlockData(filter_node.data)) {
            if (!has_pulse_output(filter_node)) {
                const flows = build_streaming_flow_to(graph, filter, signal_ast);
                return flows.map(flow => _link_graph(flow));
            }
        }
        else {
            throw new Error(`Unexpected filter block: ${JSON.stringify(filter_node)}`);
        }
    }
    else {
        skip_filter = true;
    }

    if (skip_filter) {
        const filter_ast = stepped_ast.map(b => compile_block(graph,
                                                              b.block_id,
                                                              b.arguments,
                                                              b.contents as SteppedBlockTree[],
                                                              { inside_args: false, orig_tree: null },
                                                              { before: signal_ast }
                                                             ));
        compiled_graph = [
            signal_ast,
        ].concat(filter_ast);
    }
    else {
        const filter_ast = compile_block(graph, filter.block_id, filter.arguments, [ stepped_ast, [] ],
                                         { inside_args: false, orig_tree: null },
                                         { before: signal_ast }
                                        )

        compiled_graph = [
            signal_ast,
            filter_ast,
        ];
    }

    return [_link_graph(compiled_graph)];
}

function extract_non_arguments_from_block(startBlock: CompiledBlock): CompiledBlock[] {
    // [DESTRUCTIVE]
    //
    // Finds blocks that call to functions that are not supported on arguments,
    // replaces it by a `flow-last-value` and returns them to be moved.
    const todo = [startBlock];
    const extracted = [] as CompiledBlock[];

    while (todo.length > 0) {
        const block = todo.pop();

        // Extraction is not needed for certain blocks, in these cases we'll
        // perform the block property cleanup, but won't extract the blocks.
        const skipExtraction = (block.type === 'control_wait_for_next_value');

        // Args
        let args = block.args;

        if (!args) {
            continue;
        }

        else if ((args as CompiledBlockArgMonitorDict).monitor_id) {
            continue; // Nothing can be extracted here
        }

        else if ((args as CompiledBlockServiceCallSelectorArgs).key) {
            continue; // Nothing can be extracted here
        }

        else if ((args as CompiledBlockArgCallServiceDict).service_call_values) {
            // Look into the CompiledBlockArgList
            args = (args as CompiledBlockArgCallServiceDict).service_call_values;
        }

        else if (!Array.isArray(args)) {
            // Expected an Arg list, this is the one we will use and the only one left.
            throw Error(`Unknown argument type: ${args}`);
        }

        const argList = args as CompiledBlockArgList;

        for (const arg of argList) {
            if (!arg) {
                // Empty argument
                continue;
            }
            else if (arg.type === 'constant') {
                // Nothing to do here
                continue;
            }
            else if (arg.type === 'variable' || arg.type === 'list') {
                // Nothing to do here
                continue;
            }
            else if (arg.type !== 'block') {
                throw Error(`Unknown argument type: ${arg.type}`);
            }

            let idx = -1;
            for (const block of (arg.value)) {
                idx++;

                todo.push(block);

                // Extract required operations
                const toExtract = (block.type === 'wait_for_monitor');
                if (toExtract) {
                    let valueIdx = 0;
                    const monitorDict = (block.args as CompiledBlockArgMonitorDict);
                    if (monitorDict.monitored_value || monitorDict.monitored_value === 0) {
                        valueIdx = monitorDict.monitored_value;
                        delete monitorDict.monitored_value;
                    }

                    if (!skipExtraction) {
                        extracted.push(block);

                        if (!block.id) {
                            block.id = uuidv4();
                        }

                        arg.value[idx] = {
                            type: "flow_last_value",
                            args: [
                                {
                                    type: 'constant',
                                    value: block.id,
                                },
                                {
                                    type: 'constant',
                                    value: valueIdx,
                                }
                            ],
                            contents: [],
                        };
                    }
                }
            }
        }
    }

    return extracted;
}

function extract_non_arguments_from_content(block: ContentBlock | CompiledBlock) {
    // [DESTRUCTIVE]
    //
    // Calls `extract_non_arguments` for all the contents inside `block`.
    const contents = block.contents;

    if (contents.length == 0) {
        return;
    }

    const sample = contents[0];
    if ((sample as CompiledBlock).type) {
        extract_non_arguments(contents as CompiledBlock[]);
    }
    else {
        (contents as ContentBlock[]).forEach(content => extract_non_arguments_from_content(content));
    }
}

function extract_non_arguments(flow: CompiledBlock[]) {
    // [DESTRUCTIVE]
    //
    // Finds calls to operations that cannot be resolved immediately (like
    // wait-for-monitor) that are done in arguments and DESTRUCTIVELY moves it
    // to the operations before it would be used so.
    let pos = 0;
    while (pos < flow.length) {
        const block = flow[pos];

        extract_non_arguments_from_content(block);

        const non_arguments = extract_non_arguments_from_block(block);

        // Insert the arguments reversed on the flow
        non_arguments.reverse();
        flow.splice(pos, 0, ...non_arguments);

        pos += non_arguments.length + 1;
    }
}

export function compile(graph: FlowGraph): CompiledFlowGraph[] {
    // Isolate destructive changes. These are mostly performed on
    // `lift_common_ops` and `extract_internally_reused_arguments`.
    graph = JSON.parse(JSON.stringify(graph));

    graph = lift_common_ops(graph);
    graph = extract_internally_reused_arguments(graph);
    graph = split_streaming_after_stepped(graph);

    const source_signals = get_source_signals(graph);
    const filters: BlockTree[][] = [];
    for (const signal_id of source_signals) {
        const source_filters = get_filters(graph, signal_id);
        filters.push(source_filters);
    }

    const stepped_asts = [];

    for (let i = 0; i < filters.length ; i++) {
        const subfilters = filters[i];
        let columns = [];

        if (subfilters.length > 0) {
            for (const subfilter of subfilters) {
                if (subfilter !== null) {
                    columns.push(get_stepped_ast(graph, subfilter.block_id));
                }
                else {
                    // If the source is a trigger, the filter does not exist itself
                    const ast = get_stepped_ast(graph, source_signals[i]);
                    if (ast.length > 0) {
                        // Skip triggers with no AST or filter
                        columns.push(ast);
                    }
                }
            }
        }
        else {
            // If the source is a trigger, the filter does not exist itself
            columns.push(get_stepped_ast(graph, source_signals[i]))
        }
        stepped_asts.push(columns);
    }

    // Finally assemble everything
    let flows: CompiledFlowGraph[] = [];
    for (let i = 0; i < source_signals.length; i++) {
        const signal_id = source_signals[i];

        if (filters[i].length > 0) {
            for (let j = 0; j < filters[i].length; j++) {
                const filter = filters[i][j];
                const ast = stepped_asts[i][j];

                if (ast) {
                    flows = flows.concat(assemble_flow(graph, signal_id, filter, ast));
                }
            }
        }
        else {
            const ast = stepped_asts[i][0];
            if (ast.length > 0) { // Ignore triggers with no operations
                flows = flows.concat(assemble_flow(graph, signal_id, null, ast));
            }
        }
    }

    // Extract operations that cannot be done as arguments
    flows.forEach(flow => extract_non_arguments(flow));

    // TODO: Deduplicate programs

    return flows;
}
