export interface StaticBlockArgument {
    type: string;
    default_value: string;
};

export interface DynamicBlockArgument {
    type: string;
    callback: string;
};

export type BlockArgument = StaticBlockArgument | DynamicBlockArgument;


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

function argument_to_xml(arg: BlockArgument, index: number): string {
    const type = to_scratch_type(arg.type);
    return `<value name="VAL${index}">
        <shadow type="${type}">
            <field name="${to_field_name(arg.type)}">${arg.default_value}</field>
        </shadow>
    </value>`;
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

    console.log(xml);
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
        return 'output_string'; // TODO: adapt to the appropriate type
    }
    else {
        console.log('Unknown block type:', block.block_type);
        return 'output_string'; // TODO: adapt to the appropriate type
    }
}

export function get_block_toolbox_arguments(block: ResolvedCustomBlock) {
    return block.arguments.map((arg, index, _array) => {
        if ((arg as any).options) {  // Dynamic value
            const resolved_arg = arg as ResolvedDynamicBlockArgument;
            return {
                type: 'field_dropdown',
                name: 'VAL' + index,
                options: resolved_arg.options,
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