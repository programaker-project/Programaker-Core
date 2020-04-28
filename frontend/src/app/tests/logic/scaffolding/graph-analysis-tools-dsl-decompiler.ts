import { CompiledBlock, CompiledBlockArg, CompiledBlockArgCallServiceDict, CompiledBlockArgMonitorDict, CompiledBlockArgs, CompiledFlowGraph, ContentBlock } from '../../../flow-editor/flow_graph';

import { OP_TRANSLATIONS } from './graph-analysis-tools-ast-dsl';

const OP_REVERSE_TRANSLATIONS = {};
for (const op of Object.keys(OP_TRANSLATIONS)) {
    OP_REVERSE_TRANSLATIONS[OP_TRANSLATIONS[op]] = op;
}

function repr_single_arg(arg: CompiledBlockArg, depth: number): string {
    if (arg.type === 'constant') {
        return JSON.stringify(arg.value);
    }
    else if (arg.type === 'block') {
        return repr_contents(arg.value, depth, { skip_first_indent: true });
    }
}

function repr_args(args: CompiledBlockArgs, depth: number): string {
    if (args instanceof Array) {
        return args.map(arg => repr_single_arg(arg, depth)).join(' ');
    }
    else if ((args as CompiledBlockArgMonitorDict).monitor_id) {
        const mon_args = (args as CompiledBlockArgMonitorDict);
        const value = JSON.stringify(mon_args.expected_value);
        return `from_service: "${mon_args.monitor_id.from_service}" expected_value: ${value}`
    }
    else if ((args as CompiledBlockArgCallServiceDict).service_id) {
        const call_args = (args as CompiledBlockArgCallServiceDict);
        const values = repr_args(call_args.service_call_values, depth);
        return `id: ${call_args.service_id} action: ${call_args.service_action} values: (${values})`;
    }
    else {
        throw new Error(`Unknown args type: ${args}`)
    }
}

function repr_contents(contents: (CompiledBlock|ContentBlock)[], depth: number, options?: {skip_first_indent?: boolean}): string {
    const results = [];
    for (const content of contents) {
        if ((content as CompiledBlock).type) {
            results.push(gen_tree(content as CompiledBlock, depth));
        }
        else {
            results.push(`(${repr_contents(content.contents, depth + 1, { skip_first_indent: true })})`);
        }
    }

    if (results.length === 0) {
        return '';
    }

    let prefix = '';
    if (!options || !options.skip_first_indent) {
        prefix = indentation_for_depth(depth);
    }
    return prefix + results.join('\n' + indentation_for_depth(depth));
}

function indentation_for_depth(depth: number) {
    const character = ' ';

    return Array(depth).fill(character).join('');
}

function gen_tree(block: CompiledBlock, depth: number): string {
    const args = repr_args(block.args, depth);
    let type = block.type;
    if (OP_REVERSE_TRANSLATIONS[type]) {
        type = OP_REVERSE_TRANSLATIONS[type];
    }

    const depth_increase = 1 + type.length + 2; // '(' operation ' ' * 2

    if (block.contents && block.contents.length > 0) {
        const contents = repr_contents(block.contents, depth + depth_increase);
        return `(${type} ${args}\n${contents})`;
    }
    else {
        return `(${type} ${args})`;
    }
}

function decompile_flow(flow: CompiledFlowGraph): string {
    const tokens = [];

    for (const op of flow) {
        tokens.push(gen_tree(op, 0));
    }

    return tokens.join('\n');
}

export function decompile_to_dsl(flows: CompiledFlowGraph[]): string[] {
    return flows.map(flow => decompile_flow(flow));
}
