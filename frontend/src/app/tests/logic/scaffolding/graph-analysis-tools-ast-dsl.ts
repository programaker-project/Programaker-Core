import { SimpleArrayAst, SimpleArrayAstArgument } from './graph-analysis-tools';

const SYMBOL_CHARACTERS = (
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        + '0123456789'
        + '!$\'",_-./:;?+<=>#%&*@[\]{|}`^~'
);

export const OP_TRANSLATIONS = {
    // Flow control
    'if': 'control_if_else',
    'jump-to': 'jump_to_block',
    'jump-point': 'jump_point',
    'fork': 'op_fork_execution',

    // Operations
    'log': 'logging_add_log',
    'wait-seconds': 'control_wait',
    'call-service': 'command_call_service',
    'preload': 'op_preload_getter',
    'on-block-run': 'op_on_block_run',

    // Variables
    'set-var': 'data_setvariableto',
    'on-var': 'on_data_variable_update',
    'get-var': 'data_variable',
    'flow-last-value': 'flow_last_value',
    'wait-for-monitor': 'wait_for_monitor',

    // Comparations
    '=': 'operator_equals',
    'and': 'operator_and',
    '<': 'operator_lt',
    '>': 'operator_gt',

    // Operations
    'mod': 'operator_modulo',
    '+': 'operator_add',
    'add-to-list': 'data_addtolist',
    'list-length': 'data_lengthoflist',
    'delete-list-index': 'data_deleteoflist',
};

const OPS_WITH_MAP_ARGUMENTS = [
    'wait_for_monitor', 'command_call_service',
];

function is_digit(c: string) {
    return '0123456789'.indexOf(c) >= 0;
}

function array_to_map(args: any[]): {[key: string]: any} {
    if ((args.length % 2) > 0) {
        throw new Error("Odd number of arguments on function call, expected even.");
    }

    const result = {};

    for (let idx=0; idx < args.length; idx+=2) {
        let key = args[idx];
        const value = args[idx+1];

        if (key.indexOf(':') >= 0) {
            if (key.indexOf(':') != (key.length - 1)) {
                throw new Error("Character ':' allowed ONLY at the end of symbol, not before");
            }

            key = key.substring(0, key.length - 1);
        }

        result[key] = value;
    }

    return result;
}

function transform_call(args: any[]): any[] {
    if (args.length == 0) {
        return args;
    }

    let op = args[0] as string;
    if (OP_TRANSLATIONS[op]) {
        op = args[0] = OP_TRANSLATIONS[op];
    }

    if (OPS_WITH_MAP_ARGUMENTS.indexOf(op) >= 0) {
        args = [op, array_to_map(args.slice(1))];
    }

    if (op === 'wait_for_monitor') {
        if (args[1].from_service) {
            args[1].monitor_id = { from_service: args[1].from_service };
            delete args[1].from_service;
        }
        if (!args[1].monitor_expected_value) {
            args[1].monitor_expected_value = 'any_value';
        }
    }

    if (op === 'command_call_service') {
        if (args[1].id) {
            args[1].service_id = args[1].id;
            delete args[1].id;
        }
        if (args[1].action) {
            args[1].service_action = args[1].action;
            delete args[1].action;
        }
        if (args[1].values) {
            args[1].service_call_values = args[1].values;
            delete args[1].values;
        }
    }

    if (op === 'op_fork_execution') {
        let kw_args_index: number;
        for (kw_args_index = 1; kw_args_index < args.length; kw_args_index++) {
            const arg = args[kw_args_index];

            if (typeof arg !== 'string') {
                break;
            }

            const colon_pos = arg.indexOf(':');
            if (colon_pos >= 0) {
                if (colon_pos === 0 && (arg.substring(1).indexOf(':') < 0)) {
                    args[kw_args_index] = arg.substring(1);
                }
                else if (colon_pos === (arg.length - 1)) {
                    args[kw_args_index] = arg.substring(0, arg.length - 1);
                }
                else {
                    throw new Error(`Character ':' allowed ONLY at beginning or end of symbol. On (op:${op}, arg: ${arg})`);
                }

            }
        }

        let contents = [];
        let kw_args = [];

        if (args.length > kw_args_index) {
            contents = args.splice(kw_args_index);
        }
        if (args.length > 1) {
            kw_args = args.splice(1);
        }

        // Translate direct operations (in contents) with an operation list
        for (let idx = 0; idx < contents.length; idx++) {
            if (typeof contents[idx][0] === 'string') {
                contents[idx] = [contents[idx]];
            }
        }

        args[1] = kw_args;
        args[2] = contents;
    }

    return args;
}

function read(s: string, idx: number, linenum: number, colnum: number): [SimpleArrayAstArgument, number, number, number] {
    let op: SimpleArrayAstArgument = null;

    if (s[idx] === ';') {
        // Wait for the end of the line
        do {
            idx++;
        } while (s[idx] !== '\n');
        idx++; // Jump over next \n

        colnum = 1;
        linenum++;
    }
    else if (s[idx] === '(') {
        const start = [idx, linenum, colnum];

        idx++;
        colnum++;

        op = [] as any;
        while ((idx < s.length) && (s[idx] !== ')')) {

            let token: SimpleArrayAstArgument;
            [token, idx, linenum, colnum] = read(s, idx, linenum, colnum);
            if (token !== null) {
                (op as any).push(token);
            }
        }
        if (idx >= s.length) {
            throw new Error(`${start[1]}:${start[2]} Unclosed list`);
        }
        if (s[idx] !== ')') {
            throw new Error(`${start[1]}:${start[2]} Unexpected state on list close (this should not happen)`);
        }

        try {
            op = transform_call(op as any) as any;
        }
        catch (err) {
            err.message = `On call starting at ${start[1]}:${start[2]}: ${err.message}`;
            throw err;
        }

        idx++;
        colnum++;
    }
    else if (s[idx] === ')') {
        throw new Error(`${linenum}:${colnum} Unmatched list close ')'`)
    }
    else if (s[idx] === '"') {
        const token = [];
        const start = [idx, linenum, colnum];
        let just_escaped = false;

        idx++;
        colnum++;
        while ((s[idx] !== '"' ) || just_escaped) {
            if (just_escaped) {
                // TODO: Convert current character
                just_escaped = false;
                throw new Error('NOT IMPLEMENTED');
            }
            else {
                if (s[idx] === '\\') {
                    just_escaped = true;
                }
                else if (s[idx] === '\n') {
                    throw new Error(`${linenum}:${colnum} Unended string starting at ${start[1]}:${start[2]}`)
                }
                else {
                    token.push(s[idx]);
                }
            }
            idx++;
            colnum++;

        }
        // Jump over end "
        idx++;
        colnum++;

        op = token.join('');
    }
    else if (' \t'.indexOf(s[idx]) >= 0) {
        idx++;
        colnum++; // Whitespace
    }
    else if (s[idx] === '\n') {
        idx++;
        colnum = 1;
        linenum++;
    }
    else if (SYMBOL_CHARACTERS.indexOf(s[idx]) >= 0) {
        const token = [];

        while (SYMBOL_CHARACTERS.indexOf(s[idx]) >= 0) {
            token.push(s[idx]);

            idx++;
            colnum++;
        }

        op = token.join('');
        if (token.filter(c => is_digit(c)).length === token.length) { // All are digits
            op = parseInt(op);
        }
        else if (op.match(/(^\d*\.\d+$)|(^\d+\.\d$)/)) {
            op = parseFloat(op);
        }
    }
    else {
        throw new Error(`${linenum}:${colnum} Unexpected character ${s[idx]}`)
    }

    return [op, idx, linenum, colnum];
}

export function dsl_to_ast(s: string): SimpleArrayAst {
    const ast = [];

    let linenum = 1;
    let colnum = 1;

    for (let idx = 0; idx < s.length;) {
        let token: SimpleArrayAstArgument;
        [token, idx, linenum, colnum] = read(s, idx, linenum, colnum);
        if (token !== null) {
            ast.push(token);
        }
    }

    return ast;
}
