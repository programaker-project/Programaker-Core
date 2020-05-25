type VariableClass = 'single' | 'list' | undefined;

export interface StaticBlockArgument {
    type: string;
    default_value: string;
    class: VariableClass;
};

export interface DynamicBlockArgument {
    type: string;
    callback: string;
};

export type BlockArgument = StaticBlockArgument | DynamicBlockArgument;

interface ScratchSerializableArgument {
    type: string,
    name: string,
    variableTypes?: string[]
};

interface ScratchImageArgument {
    type: 'field_image',
    src: string,
    width: number,
    height: number,
    alt?: string,
    flip_rtl?: boolean,
};

interface ScratchLabelArgument {
    type: 'field_label',
    text: string,
}

export type ScratchBlockArgument = ScratchSerializableArgument | ScratchImageArgument | ScratchLabelArgument;

export interface ResolvedDynamicBlockArgument {
    type: string;
    callback: string;
    options: [string, string][];
};

export type ResolvedBlockArgument = StaticBlockArgument | ResolvedDynamicBlockArgument;

export interface CustomBlock {
    id: string;
    service_port_id: string;
    block_id: string;
    block_type: string;
    block_result_type: string;
    function_name: string;
    message: string;
    arguments: BlockArgument[];
    save_to: undefined | { "type": "argument", "index": number };
};

export interface ResolvedCustomBlock {
    id: string;
    service_port_id: string;
    block_id: string;
    block_type: string;
    block_result_type: string;
    function_name: string;
    message: string;
    arguments: ResolvedBlockArgument[];
    save_to: undefined | { "type": "argument", "index": number };
}

export interface CategorizedCustomBlock {
    bridge_data: BridgeData;
    resolved_custom_blocks: ResolvedCustomBlock[];
}

export interface BridgeData {
    bridge_name: string;
    bridge_id: string;
}

function to_scratch_type(type) {
    switch (type) {
        case 'string':
            return "text";

        case 'integer':
            return 'math_whole_number';

        case 'float':
            return 'math_number';

        default:
            return 'text';
    }
}

function to_field_name(type) {
    switch (type) {
        case 'string':
            return "TEXT";

        case 'integer': case 'float':
            return 'NUM';

        default:
            return 'TEXT';
    }
}

function get_argument_type_default_value(type: string): any {
    switch (type){
        case 'string':
            return 'Sample';

        case 'integer': case 'float':
            return 1;

        default:
            return 'Sample';
    }
}

function argument_to_xml(arg: BlockArgument, index: number): string {
    const type = to_scratch_type(arg.type);

    let default_value = (arg as StaticBlockArgument).default_value;

    if ((default_value === null) || (default_value === '') || (default_value === undefined)) {

        default_value = get_argument_type_default_value((arg as StaticBlockArgument).type);
    }

    if ((arg as StaticBlockArgument).class === 'list') {
        return `
        <field name="VAL${index}" variabletype="list" id=""></field>
        `;
    }
    else {
        return `<value name="VAL${index}">
            <shadow type="${type}">
                <field name="${to_field_name(arg.type)}">${default_value}</field>
            </shadow>
        </value>`;
    }
}

export function block_to_xml(block: CustomBlock): string {
    const full_id = JSON.stringify(block.id);
    const values = block.arguments.map((argument, index, _array) => {
        return argument_to_xml(argument, index);
    });

    const xml = `<block type=${full_id} id=${full_id}>
    ${values.join()}
    </block>
    `;

    return xml;
}

export function get_block_category(block: CustomBlock): string {
    if (block.block_type == 'trigger') {
        return 'shape_hat';
    }
    else if (block.block_type == 'operation') {
        return 'shape_statement';
    }
    else if (block.block_type == 'getter') {
        switch (block.block_result_type) {
            case 'boolean':
                return 'output_boolean';
            case 'number':
                return 'output_boolean';
            default:
                return 'output_string';
        }
    }
    else {
        console.log('Unknown block type:', block.block_type);
        return 'output_string'; // TODO: adapt to the appropriate type
    }
}

export function get_block_toolbox_arguments(block: ResolvedCustomBlock): ScratchBlockArgument[] {
    return block.arguments.map((arg, index, _array) => {
        if ((arg as any).options) {  // Dynamic value
            const resolved_arg = arg as ResolvedDynamicBlockArgument;
            return {
                type: 'field_dropdown',
                name: 'VAL' + index,
                options: resolved_arg.options,
            };
        }
        else if (arg.type === 'variable') {
            if ((arg as StaticBlockArgument).class === 'list') {
                return {
                    "type": "field_variable",
                    "name": "LIST" + index,
                    "variableTypes": ["list"],  // actually, Blockly.LIST_VARIABLE_TYPE
                };
            }
            return {
                'type': 'field_variable',
                'name': 'VAL' + index,
            };
        }
        else {
            return {
                type: 'input_value',
                name: 'VAL' + index,
            };
        }
    });
}
