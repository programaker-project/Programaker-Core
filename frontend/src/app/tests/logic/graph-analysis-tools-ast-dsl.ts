import { SimpleArrayAst, SimpleArrayAstArgument, SimpleArrayAstOperation } from './graph-analysis-tools';

const SYMBOL_CHARACTERS = (
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        + '0123456789'
        + '!$\'",_-./:;?+<=>#%&*@[\]{|}`^~'
);

const OP_TRANSLATIONS = {
    'if': 'control_if_else',
    '=': 'operator_equals',
    'and': 'operator_and',
    'wait-seconds': 'op_wait_seconds',
    'wait-for-monitor': 'wait_for_monitor',
    'call-service': 'command_call_service',
    'flow-last-value': 'flow_last_value',
    'jump-to': 'jump_to_block',
    'jump-point': 'jump_point',
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
        if (!args[1].expected_value) {
            args[1].expected_value = 'any_value';
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
            throw new Error(`${start[1]}:${start[2]} ` + err.message);
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
