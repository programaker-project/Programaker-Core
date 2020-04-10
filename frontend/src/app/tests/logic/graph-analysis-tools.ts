import { CompiledFlowGraph, CompiledBlock, CompiledBlockArg, ContentBlock } from '../../flow-editor/flow_graph';

type _AndOp = ['operator_and', SimpleArrayAstArgument, SimpleArrayAstArgument];
type _EqualsOp = ['operator_equals', SimpleArrayAstArgument, SimpleArrayAstArgument]
    | ['operator_equals', SimpleArrayAstArgument, SimpleArrayAstArgument, SimpleArrayAstArgument];
type _CallServiceOp = ['command_call_service',
                       { service_id: string, service_action: string, service_call_values: SimpleArrayAstArgument[] }];
type _WaitForMonitorOp = ['wait_for_monitor', { monitor_id: { from_service: string }, expected_value: any }];
type _LastValueOp = ['flow_last_value', string, number | string];
type _IfElseOp = ['control_if_else', SimpleArrayAstArgument, SimpleArrayAstOperation[]];

export type SimpleArrayAstArgument = SimpleArrayAstOperation | string | number
export type SimpleArrayAstOperation = _AndOp | _EqualsOp
    | _CallServiceOp
    | _WaitForMonitorOp | _LastValueOp
    | _IfElseOp
;

export type SimpleArrayAst = SimpleArrayAstOperation[];

function convert_argument(arg: SimpleArrayAstArgument): CompiledBlockArg {
    if ((typeof arg === 'string') || (typeof arg === 'number')) {
        return {
            type: 'constant',
            value: arg + '',
        };
    }

    return {
        type: 'block',
        value: [convert_operation(arg)],
    };
}

function convert_content_single(op: SimpleArrayAstOperation): (CompiledBlock | ContentBlock) {
    return convert_operation(op as SimpleArrayAstOperation);
}

function convert_contents(contents: SimpleArrayAstOperation[]): ContentBlock {
    return {
        contents: contents.map(v => convert_content_single(v))
    }
}

function convert_operation(op: SimpleArrayAstOperation): CompiledBlock {

    if (op[0] === 'wait_for_monitor') {
        return {
            type: op[0],
            args: op[1],
            contents: [],
        }
    }

    if (op[0] === 'command_call_service') {
        return {
            type: op[0],
            args: {
                service_action: op[1].service_action,
                service_id: op[1].service_id,
                service_call_values: op[1].service_call_values.map(v => convert_argument(v))
            },
            contents: [],
        }
    }

    if (op[0] === 'control_if_else') {
        const contents = (op.slice(2) as SimpleArrayAstOperation[][]).map(v => convert_contents(v));

        if (contents.length < 2) {
            contents.push({ contents: [] });
        }

        return {
            type: op[0],
            args: [ convert_argument(op[1]) ],
            contents: contents,
        }
    }

    const args: SimpleArrayAstArgument[] = op.slice(1);

    return {
        type: op[0],
        args: args.map(v => convert_argument(v)),
        contents: []
    }
}

export function gen_compiled(ast: SimpleArrayAst): CompiledFlowGraph {
    const result = ast.map(v => convert_operation(v));
    return result;
}
