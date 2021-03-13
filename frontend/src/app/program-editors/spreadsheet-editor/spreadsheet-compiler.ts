import { AtomicFlowBlockOperationType, BLOCK_TYPE as ATOMIC_BLOCK_TYPE, AtomicFlowBlockOptions } from '../../flow-editor/atomic_flow_block';
import { BLOCK_TYPE as VALUE_BLOCK_TYPE } from '../../flow-editor/direct_value';
import { CompiledFlowGraph, FlowGraph } from "../../flow-editor/flow_graph";
import { compile } from "../../flow-editor/graph_analysis";
import { ISpreadsheetToolbox } from "./spreadsheet-toolbox";
import { uuidv4 } from '../../flow-editor/utils';

export function colName(index: number) {
    const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    index -= 1;
    if (letters[index]) {
        return letters[index];
    }
    else {
        return letters[index / letters.length] + letters[index % letters.length];
    }
}

type SpreadsheetConstant = {
    type: 'constant',
    value: any,
};

type SpreadsheetCall = {
    type: 'call',
    func_name: string,
    arguments: SpreadsheetOperation[],
}

type SpreadsheetCellRef = {
    type: 'cell-ref',
    value: string,
}

type SpreadsheetOperation = SpreadsheetConstant | SpreadsheetCall | SpreadsheetCellRef;

const INFIX_CHARACTERS = [ '+' ];
const STRING_CHARS = [ "'", '"' ];

// See: https://regex101.com/r/1cZtZG/5
const TOKENIZE_REGEX = new RegExp(
    /^((?<call>(?<function>[\da-z_]+)\((?<args>.*)\))|(?<string>"([^"]|\")*")|(?<whitespace>(\s*))|(?<infix>\+)|(?<cell>[A-Z]{1,2}\d+)|(?<number>-?\d+))$/
);

// See: https://regex101.com/r/3aGdXJ/1/
const ARGUMENT_REGEX = new RegExp(
    /^(?<function>(?<fun_name>[\da-z_]+)(?<fun_parens>\((.*)\)))|(?<string>"([^"]|\")*")|(?<infix>\+)|(?<cell>[A-Z]{1,2}\d+)|(?<number>\d+)(?<remaining>.*)/
);



function parse_op(data: string): SpreadsheetOperation {
    const tokens = parse_tokens(data);
    if (tokens.length !==  1) {
        throw Error(`Unexpected number of tokens (expected 1, found ${tokens.length}): ${data}`);
    }
    return tokens[0];
}

function parse_tokens(data: string): SpreadsheetOperation[] {
    data = data.trim();
    const operations = [] as SpreadsheetOperation[];
    let tokens_in_arg = [] as SpreadsheetOperation[];

    let in_string_char = null;
    let escaped = false;
    let current_token = [] as string[];
    let next_on = tokens_in_arg;
    let next_on_swap_pos = 0;
    let arg_start_pos = 0;
    let m;
    for (let idx = 0; idx < data.length; idx++){
        if (data[idx] === ' ') {
            // Ignore
        }
        else if (data[idx] === ',') {
            if (next_on !== tokens_in_arg) {
                throw Error(`Uncomplete infix operation: ${data.substr(next_on_swap_pos)}`);
            }
            if (tokens_in_arg.length != 1) {
                throw Error(`Unexpected number of tokens in arg (expected 1, found ${tokens_in_arg.length}): ${data.substr(arg_start_pos)}`);
            }
            operations.push(tokens_in_arg[0]);
            tokens_in_arg = [];
            next_on = tokens_in_arg;

            arg_start_pos = idx + 1;
        }
        else if (m = data.substr(idx).match(/^(?<name>[a-zA-Z0-9_]+)\(/)) {
            // Function call
            console.log('m', m);
            const name = m.groups["name"];

            const call_start_idx = idx;
            const arg_start_idx = idx + name.length + 1;
            idx = arg_start_idx - 1;
            let parens_count = 0;

            console.log("X", data[idx], idx);

            // Find the end of the function call
            for(; idx < data.length; idx++) {
                if (in_string_char) {
                    if (escaped) {
                        escaped = false;
                    }
                    else if (data[idx] === '\\') {
                        escaped = true;
                    }
                    else if (data[idx] === in_string_char) {
                        in_string_char = null;
                    }
                }
                else if (STRING_CHARS.indexOf(data[idx]) >= 0) {
                    in_string_char = data[idx];
                }
                else if (data[idx] === '(') {
                    parens_count++;
                    console.log(">", parens_count);
                }
                else if (data[idx] === ')') {
                    parens_count--;
                    console.log("<", parens_count);
                    if (parens_count === 0) {
                        break;
                    }
                }
            }

            console.log("IDX", idx);

            if (idx === data.length){
                throw Error(`Uncomplete call: ${data.substr(call_start_idx)}`);
            }

            console.log('=>', name, data.substring(arg_start_idx, idx));
            const args = arg_start_idx < idx
                ? parse_tokens(data.substring(arg_start_idx, idx))
                : [];

            next_on.push({
                type: 'call',
                func_name: name,
                arguments: args,
            });
            next_on = tokens_in_arg;
        }
        else if (STRING_CHARS.indexOf(data[idx]) >= 0) {
            // Parse string
            in_string_char = data[idx];
            while (in_string_char) {
                idx++;
                if (escaped) {
                    current_token.push(escape_char(data[idx]));
                    escaped = false;
                }
                else if (data[idx] === '\\') {
                    escaped = true;
                }
                else if (data[idx] === in_string_char) {
                    next_on.push({
                        type: 'constant',
                        value: current_token.join(''),
                    });
                    next_on = tokens_in_arg;
                    current_token = [];
                    in_string_char = null;
                }
            }
        }
        else if (m = data.substr(idx).match(/^[A-Z]{1,2}\d+/)) {
            let token = m[0];
            next_on.push({
                type: 'cell-ref',
                value: token,
            });
            next_on = tokens_in_arg;
            idx += token.length - 1;
        }
        else if (m = data.substr(idx).match(/^\d+/)) {
            let token = m[0];
            next_on.push({
                type: 'constant',
                value: parseFloat(token),
            });
            next_on = tokens_in_arg;
            idx += token.length - 1;
        }
        else if (data[idx] === '+') {
            // Note that this does not take into account
            const left = tokens_in_arg.pop();
            const infix_op: SpreadsheetOperation = {
                type: 'call',
                func_name: get_infix_operation(data[idx]),
                arguments: [
                    left,
                ]
            };
            next_on.push(infix_op);
            next_on = infix_op.arguments;
            next_on_swap_pos = idx;
        }
        else {
            throw Error(`Error parsing: [${data.substr(0, idx)}] → [${data.substr(idx)}]`);
        }
    }

    console.log("OP", operations);
    console.log("TOK", tokens_in_arg);
    if (next_on !== tokens_in_arg) {
        throw Error(`Uncomplete infix operation: ${data.substr(next_on_swap_pos)}`);
    }
    if (tokens_in_arg.length != 1) {
        throw Error(`Unexpected number of tokens in arg (expected 1, found ${tokens_in_arg.length}): [${data.substr(0,arg_start_pos)}] → [${data.substr(arg_start_pos)}]`);
    }
    operations.push(tokens_in_arg[0]);

    return operations;
}

function escape_char(char: string): string {
    const escape_mapping: {[key: string]: string} = {
        '"': '"',
        "'": "'",
        "n": '\n',
        "r": '\r',
        "\\": '\\',
    };

    return escape_mapping[char]
}

function get_infix_operation(char: string): string {
    const mapping: {[key: string]: string} = {
        '+': 'operator_add',
    };

    return mapping[char]
}

function parse_cell(data: string): SpreadsheetOperation {
    data = data.trim();
    if (!data.startsWith('=')) {
        return {
            type: 'constant',
            value: data
        }
    }

    return parse_op(data.substr(1));
}


export function build_graph(orig: {[key: string]: string}, toolbox: ISpreadsheetToolbox): FlowGraph {
    const g: FlowGraph = { nodes: {}, edges: [] };

    const ops = [];
    for (const id of Object.keys(orig)) {
        const data = orig[id];
        const op = parse_cell(data);

        add_op_to_graph(op, toolbox, g, id);
    }

    return g;
}

function add_op_to_graph(op: SpreadsheetOperation, toolbox: ISpreadsheetToolbox, g: FlowGraph, id: string ) {
    if (op.type === 'constant') {
        g.nodes[id] = {
            data: {
                type: VALUE_BLOCK_TYPE,
                value: op.value,
            }
        }
        return;
    }
    else if (op.type === 'cell-ref') {
        throw Error("CELL-REF not implemented as operation");
    }

    console.log(op.func_name, op, toolbox.blockMap[op.func_name])
    const call = toolbox.blockMap[op.func_name];
    let block = {
        block_type: 'getter',
        message: op.func_name,
        id: op.func_name,
    };

    if (call) {
        block = call.block;
    }

    g.nodes[id] = {
        data: {
            type: ATOMIC_BLOCK_TYPE,
            value: {
                options: {
                    type: block.block_type as AtomicFlowBlockOperationType,
                    block_function: block.id,
                    message: block.message,
                } as AtomicFlowBlockOptions,
                slots: {},
                synthetic_input_count: 0,
                synthetic_output_count: 0,
            }
        }
    }

    let idx = -1;
    for (const arg of op.arguments) {
        idx++; // First argument is index: 0

        if (arg.type === 'cell-ref') {
            // Cells are referenced directly, to avoid unnecessary intermediate steps
            g.edges.push({
                from: {
                    id: arg.value,
                    output_index: 0,
                },
                to: {
                    id: id,
                    input_index: idx,
                }
            });
            continue;
        }

        const arg_id = uuidv4();
        add_op_to_graph(arg, toolbox, g, arg_id);
        g.edges.push({
            from: {
                id: arg_id,
                output_index: 0,
            },
            to: {
                id: id,
                input_index: idx,
            }
        });
    }
}

export function compile_spreadsheet(orig: {[key: string]: string}, toolbox: ISpreadsheetToolbox): CompiledFlowGraph[] {
    const graph = build_graph(orig, toolbox);

    console.log("Graph: \n" + JSON.stringify(graph, null, 4));

    return compile(graph);
}
