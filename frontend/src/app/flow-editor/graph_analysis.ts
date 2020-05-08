import { AtomicFlowBlock, AtomicFlowBlockData, AtomicFlowBlockOptions } from './atomic_flow_block';
import { BaseToolboxDescription, ToolboxDescription } from './base_toolbox_description';
import { DirectValue, DirectValueFlowBlockData } from './direct_value';
import { EnumDirectValue, EnumDirectValueFlowBlockData } from './enum_direct_value';
import { CompiledBlock, CompiledBlockArg, CompiledBlockArgs, CompiledFlowGraph, ContentBlock, FlowGraph, FlowGraphEdge, FlowGraphNode } from './flow_graph';
import { extract_internally_reused_arguments, is_pulse_output, lift_common_ops, scan_downstream, scan_upstream } from './graph_transformations';
import { index_connections, reverse_index_connections } from './graph_utils';
import { TIME_MONITOR_ID } from './platform_facilities';
import { uuidv4 } from './utils';

function index_toolbox_description(desc: ToolboxDescription): {[key: string]: AtomicFlowBlockOptions} {
    const result: {[key: string]: AtomicFlowBlockOptions} = {};

    for (const cat of desc) {
        for (const block of cat.blocks) {
            result[block.block_function] = block;
        }
    }

    return result;
}

const BASE_TOOLBOX_BLOCKS = index_toolbox_description(BaseToolboxDescription);

const JUMP_TO_POSITION_OPERATION = 'jump_to_position';
const JUMP_TO_BLOCK_OPERATION = 'jump_to_block';
const FORK_OPERATION = 'op_fork_execution';

function makes_reachable(conn: FlowGraphEdge, block: FlowGraphNode): boolean {
    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

        if (data.value.options.type !== 'operation') {
            // Getter or trigger
            return true;
        }

        const input = data.value.options.inputs[conn.to.input_index];
        if (!input) {
            const extras = data.value.options.extra_inputs;
            if (extras) {
                return extras.type === 'pulse';
            }

            throw new Error(`No input #${conn.to.input_index} on ${JSON.stringify(data.value.options.inputs)} [conn: ${JSON.stringify(conn)}]`);
        }

        return input.type === 'pulse';
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        throw new Error('Connection from reached block to value (backwards?)');
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
        throw new Error('Connection from reached block to value (backwards?)');
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

function is_getter_node(block: FlowGraphNode): boolean {
    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

        if (data.value.options.type !== 'operation') {
            // Getter or trigger
            return true;
        }
        return false;
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        return true;
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
        return true;
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
        const block = graph.nodes[block_id];
        if (block.data.type === AtomicFlowBlock.GetBlockType()){
            const data = block.data as AtomicFlowBlockData;

            const inputs = data.value.options.inputs || [];

            // If it has any pulse input, it's not a source block
            if (inputs.filter(v => v.type === 'pulse').length > 0) {
                continue;
            }

            // If it has a getter input, it's not a source block (the getter is)
            let has_block_inputs = false;
            for (const conn of rev_conn_index[block_id] || []) {
                const orig = graph.nodes[conn.from.id];
                if (orig.data.type === AtomicFlowBlock.GetBlockType()) {
                    const a_block = orig.data as AtomicFlowBlockData;
                    if (a_block.value.options.type === 'getter') {
                        has_block_inputs = true;
                        break;
                    }
                    else {
                        throw new Error(`Unexpected input block to trigger (id: ${conn.from.id}, type: ${a_block.value.options.type}), expected a "getter"`);
                    }
                }
            }

            if (has_block_inputs) {
                continue;
            }

            // If it's a getter check that it gets derived into a trigger
            if (data.value.options.type === 'getter') {
                const find_triggers_downstream_controller = ((_node_id: string, node: FlowGraphNode, _: string[]) => {
                    if (node.data.type === AtomicFlowBlock.GetBlockType()) {
                        const a_node = node.data as AtomicFlowBlockData;

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
                    else {
                        throw new Error(`Unexpected: Direct value block should not have an input`);
                    }
                });

                if (!scan_downstream(graph, block_id, conn_index, find_triggers_downstream_controller)) {
                    continue; // Ignore this entry if id doesn't derive into a trigger
                }
            }

            signals.push(block_id);
        }
    }

    return signals;
}

function has_pulse_output(block: FlowGraphNode): boolean {
    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

        const outputs = data.value.options.outputs || [];

        // If it has no pulse inputs its a source block
        return outputs.filter(v => v.type === 'pulse').length > 0;
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

function has_pulse_input(block: FlowGraphNode): boolean {
    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

        const inputs = data.value.options.inputs || [];

        // If it has no pulse inputs its a source block
        return inputs.filter(v => v.type === 'pulse').length > 0;
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        return false;
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
        return false;
    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

export function get_conversions_to_stepped(graph: FlowGraph, source_block_id: string): string[] {
    const results = {};
    const reached = build_index([source_block_id]);

    let remaining_connections: FlowGraphEdge[] = [].concat(graph.edges);
    let empty_pass = false;

    do {
        empty_pass = true;

        const skipped: FlowGraphEdge[] = [];
        for (const conn of remaining_connections) {
            if (reached[conn.from.id]) {
                const node = graph.nodes[conn.to.id];
                if (has_pulse_output(node) && !has_pulse_input(node)) {
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
        return get_tree_with_ends(graph, source_block_id, bottom_id)
    });
}

export function get_stepped_block_arguments(graph: FlowGraph, block_id: string): BlockTreeArgument[] {
    const args: BlockTreeArgument[] = [];

    let pulse_offset = 0;
    let pulse_ports = {};

    for (const conn of graph.edges) {

        if (conn.to.id === block_id) {
            if (is_pulse_output(graph.nodes[conn.from.id], conn.from.output_index)) {
                pulse_ports[conn.to.input_index] = true;
                pulse_offset = Math.max(pulse_offset, conn.to.input_index + 1);
            }
            else {
                pulse_ports[conn.to.input_index] = false;

                if (args[conn.to.input_index] !== undefined) {
                    throw new Error("Multiple inputs on single port");
                }

                args[conn.to.input_index] = {
                    tree: {
                        block_id: conn.from.id,
                        arguments: get_stepped_block_arguments(graph, conn.from.id),
                    },
                    output_index: conn.from.output_index,
                };
            }
        }
    }

    // Validate that all pulse's are grouped
    for (let i = 0; i < pulse_offset; i++) {
        if (pulse_ports[i]) {
            args.shift();
        }
        else {
            throw new Error(`Non-pulse input before a pulse one on block_id:${block_id}`);
        }
    }

    return args;
}

function get_stepped_ast_continuation(graph: FlowGraph,
                                      continuation: FlowGraphEdge,
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
            arguments: get_stepped_block_arguments(graph, block_id),
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

    throw new Error(`Block (id: ${block_id}) not found`);
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

        const found_blocks = {};

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
        const inserted_asts = {};
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

    if (continuations.length > 1) {
        let contents = [];

        for (const cont of continuations) {
            const subast = [];

            if (!cont || cont.length === 0) {
                contents.push([]);
            }
            else if (cont.length > 1) {
                throw new Error(`There should be one and only one pulse per output`)
            }
            else {
                const subreached = Object.assign({}, reached)
                get_stepped_ast_continuation(graph, cont[0], subast, subreached);
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

        let common_suffix = [];
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
            const is_atomic_block = block.data.type === AtomicFlowBlock.GetBlockType();
            let atom_data: AtomicFlowBlockData = null;
            let func_name: string = null;
            if (is_atomic_block) {
                atom_data = block.data as AtomicFlowBlockData;
                func_name = atom_data.value.options.block_function;
            }

            if (is_atomic_block && func_name === 'control_if_else') {
                // IF statement with only one output
                const contents = [];
                get_stepped_ast_continuation(graph, continuations[0][0], contents, reached);

                ast[ast.length - 1].contents = [contents, []]; // Update parent block contents
            }
            else {
                get_stepped_ast_continuation(graph, continuations[0][0], ast, reached);
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

function compile_arg(graph: FlowGraph, arg: BlockTreeArgument, parent: string, orig: string): CompiledBlockArg {
    const block = graph.nodes[arg.tree.block_id];

    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

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
                                value: arg.output_index + '',
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
                                       { inside_args: true, orig_tree: null },
                                       { before: null, exec_orig: orig },
                                      ) ]
            }
        }
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        const data = block.data as DirectValueFlowBlockData;

        return {
            type: 'constant',
            value: data.value.value,
        };
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
        const data = block.data as EnumDirectValueFlowBlockData;

        return {
            type: 'constant',
            value: data.value.value_id,
        };
    }
    else {
        throw new Error("Unknown block type: " + block.data.type)
    }
}

function get_latest_blocks_on_each_ast_level(block: CompiledBlock): CompiledBlock[] {
    const results = [];
    const levels = [block.contents];

    while (levels.length > 0) {
        const next = levels.pop();
        if (next.length === 0) {
            continue;
        }

        if ((next[0] as CompiledBlock).type) {
            // Are compiled blocks, so take the last and add to level
            const latest = next[next.length - 1];
            results.push(latest);
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
                       flags: { inside_args: boolean, orig_tree: SteppedBlockTree },
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

    if (block.data.type === AtomicFlowBlock.GetBlockType()){
        const data = block.data as AtomicFlowBlockData;

        let compiled_contents = [];
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
                                                                         relatives.exec_orig ? relatives.exec_orig : block_id));
        const block_fun = data.value.options.block_function;

        if (block_fun === 'flow_utc_time') {
            block_type = "wait_for_monitor";
            compiled_args = {
                monitor_id: {
                    from_service: TIME_MONITOR_ID,
                },
                expected_value: 'any_value',
            };
        }
        else if (block_fun.startsWith('services.')) {
            if (data.value.options.type === 'trigger') {
                // Ignore
                block_type = block_fun;
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
            type: block_type,
            args: compiled_args,
            contents: compiled_contents,
        };
    }
    else if (block.data.type === DirectValue.GetBlockType()){
        const data = block.data as DirectValueFlowBlockData;

        if (contents.length > 0) {
            throw new Error("AssertionError: Contents.length > 0 in DirectValue block")
        }

        return {
            type: 'constant',
            value: data.value.value,

            // Not really a compiled block, this would be an argument, but this simplifies things
        } as any as CompiledBlock;
    }
    else if (block.data.type === EnumDirectValue.GetBlockType()){
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
            index[block.args[0].value] = prefix.concat([idx])

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

            const link = (op as CompiledBlock).args[0].value as string;
            block.type = JUMP_TO_POSITION_OPERATION;
            if (!positions[link]) {
                throw new Error(`Cannot link to "${link}"`)
            }

            block.args[0].value = positions[link];
        }

        link_jumps(op.contents || [], positions);
    }
}

export function _link_graph(graph: CompiledFlowGraph): CompiledFlowGraph {
    const block_id_index = process_graph_jump_index(graph);

    link_jumps(graph, block_id_index);

    return graph;
}

export function assemble_flow(graph: FlowGraph,
                              signal_id: string,
                              filter: BlockTree,
                              stepped_ast: SteppedBlockTree[]): CompiledFlowGraph {

    const signal_ast = compile_block(graph, signal_id, [], [],
                                     { inside_args: false, orig_tree: null },
                                     { before: null });

    let skip_filter = false;
    let compiled_graph = null;

    if (filter) {
        const filter_node = graph.nodes[filter.block_id];

        if (filter_node.data.type === AtomicFlowBlock.GetBlockType()) {
            const f_block = (filter_node.data as AtomicFlowBlockData);
            if (f_block.value.options.block_function === 'trigger_on_signal') {
                skip_filter = true;
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

    return _link_graph(compiled_graph);
}

export function compile(graph: FlowGraph): CompiledFlowGraph[] {
    // Isolate destructive changes. These are mostly performed on
    // `lift_common_ops` and `extract_internally_reused_arguments`.
    graph = JSON.parse(JSON.stringify(graph));

    graph = lift_common_ops(graph);
    graph = extract_internally_reused_arguments(graph);

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
                columns.push(get_stepped_ast(graph, subfilter.block_id));
            }
        }
        else {
            // If the source is a trigger, the filter does not exist itself
            columns.push(get_stepped_ast(graph, source_signals[i]))
        }
        stepped_asts.push(columns);
    }

    // Finally assemble everything
    const flows: CompiledFlowGraph[] = [];
    for (let i = 0; i < source_signals.length; i++) {
        const signal_id = source_signals[i];

        if (filters[i].length > 0) {
            for (let j = 0; j < filters[i].length; j++) {
                const filter = filters[i][j];
                const ast = stepped_asts[i][j];
                flows.push(assemble_flow(graph, signal_id, filter, ast));
            }
        }
        else {
            const ast = stepped_asts[i][0];
            flows.push(assemble_flow(graph, signal_id, null, ast));
        }
    }

    return flows;
}
