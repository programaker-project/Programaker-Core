import { AtomicFlowBlockOptions } from './atomic_flow_block';
import { PLATFORM_ICON } from './definitions';
import { UiFlowBlockOptions } from './ui-blocks/ui_flow_block';
import { ContainerFlowBlockOptions } from './ui-blocks/container_flow_block';

interface Category {
    id: string,
    name: string,
    blocks: ((AtomicFlowBlockOptions | UiFlowBlockOptions | ContainerFlowBlockOptions) & { is_internal?: boolean })[],
}

export type ToolboxDescription = Category[];

export const OP_PRELOAD_BLOCK: AtomicFlowBlockOptions = {
    icon: PLATFORM_ICON,
    message: 'Preload getter',
    block_function: 'op_preload_getter',
    type: 'operation',
    inputs: [
        {
            name: "getter",
            type: "any",
        },
    ],
    outputs: [],
};

export const OP_ON_BLOCK_RUN: AtomicFlowBlockOptions = {
    icon: PLATFORM_ICON,
    message: 'On block run',
    block_function: 'op_on_block_run',
    type: 'trigger',
    inputs: [
        {
            name: "block_id",
            type: "string",
        },
        {
            name: "block_id",
            type: "integer",
        },
    ],
    outputs: [],
};

export const ADVANCED_CATEGORY = 'advanced';
export const INTERNAL_CATEGORY = '__internal__';

export const BaseToolboxDescription: ToolboxDescription = [
    {
        id: 'control',
        name: 'Control',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'Wait',
                block_function: 'control_wait',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        name: "seconds to wait",
                        type: "integer",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Broadcast to all users',
                block_function: 'control_broadcast_to_all_users',
                type: 'operation',
                fixed_pulses: true,
                inputs: [
                    {
                        required: true,
                        type: "user-pulse",
                    },
                ],
                outputs: [
                    {
                        type: "pulse",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Check',
                block_function: 'control_if_else',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        name: "check",
                        type: "boolean",
                    },
                ],
                outputs: [
                    {
                        name: 'if true',
                        type: 'pulse',
                    },
                    {
                        name: 'if false',
                        type: 'pulse',
                    }
                ],
            },
            {
                icon: PLATFORM_ICON,
                message: 'Wait for %i1 to be true',
                block_function: 'control_wait_until',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        name: "check",
                        type: "boolean",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'On new %i1 value',
                block_function: 'trigger_on_signal',
                type: 'trigger',
                inputs: [
                    {
                        required: true,
                        name: "signal",
                        type: "any",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Wait for pulse %i1 before passing signal %i2',
                block_function: 'control_signal_wait_for_pulse',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: "pulse",
                        type: "pulse",
                    },
                    {
                        required: true,
                        name: "signal",
                        type: "any",
                    },
                ],
                outputs: [
                    {
                        type: "any",
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Wait for next value',
                block_function: 'control_wait_for_next_value',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        type: "any",
                    },
                ],
                outputs: [
                    {
                        name: "value",
                        type: "any",
                    }
                ],
            },
            {
                icon: PLATFORM_ICON,
                message: 'Repeat times',
                block_function: 'control_repeat',
                type: 'operation',
                inputs: [
                    {
                        name: "start loop",
                        type: "pulse",
                    },
                    {
                        required: true,
                        name: "repetition times",
                        type: "integer",
                    },
                ],
                outputs: [
                    {
                        name: "loop continues",
                        type: "pulse",
                    },
                    {
                        name: "iteration #",
                        type: "integer",
                    },
                    {
                        name: "loop completed",
                        type: "pulse",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'When all true',
                block_function: 'trigger_when_all_true',
                type: 'trigger',
                inputs: [
                    {
                        type: "boolean",
                    },
                    {
                        type: "boolean",
                    },
                ],
                extra_inputs: {
                    type: "boolean",
                    quantity: "any",
                },
            },
            {
                icon: PLATFORM_ICON,
                message: 'Run on parallel',
                block_function: 'op_fork_execution',
                type: 'operation',
                outputs: [
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                ],
                // TODO: Implement extra_outputs
                // extra_outputs: {
                //     type: "boolean",
                //     quantity: "any",
                // },
            },
            {
                icon: PLATFORM_ICON,
                message: 'When all completed',
                block_function: 'trigger_when_all_completed',
                type: 'trigger',
                inputs: [
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                ],
                extra_inputs: {
                    type: "pulse",
                    quantity: "any",
                },
            },
            {
                icon: PLATFORM_ICON,
                message: 'When first completed',
                block_function: 'trigger_when_first_completed',
                type: 'trigger',
                inputs: [
                    {
                        type: "pulse",
                    },
                    {
                        type: "pulse",
                    },
                ],
                extra_inputs: {
                    type: "pulse",
                    quantity: "any",
                },
            }
        ]
    },
    {
        id: 'operators',
        name: 'Operators',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: '%i1 + %i2',
                block_function: 'operator_add',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "float",
                    },
                    {
                        required: true,
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'float',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: '%i1 - %i2',
                block_function: 'operator_subtract',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "float",
                    },
                    {
                        required: true,
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'float',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: '%i1 × %i2',
                block_function: 'operator_multiply',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "float",
                    },
                    {
                        required: true,
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'float',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: '%i1 / %i2',
                block_function: 'operator_divide',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: 'dividend',
                        type: "float",
                    },
                    {
                        required: true,
                        name: 'divisor',
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'float',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: '%i1 modulo %i2',
                block_function: 'operator_modulo',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: 'dividend',
                        type: "float",
                    },
                    {
                        required: true,
                        name: 'divisor',
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'float',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Is %i1 greater (>) than %i2 ?',
                block_function: 'operator_gt',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: "bigger",
                        type: "float",
                    },
                    {
                        required: true,
                        name: "smaller",
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'boolean',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Are all equals?',
                block_function: 'operator_equals',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "any",
                    },
                    {
                        required: true,
                        type: "any",
                    },
                ],
                extra_inputs: {
                    type: "any",
                    quantity: "any",
                },
                outputs: [
                    {
                        type: 'boolean',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Is %i1 less (<) than %i2?',
                block_function: 'operator_lt',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: "smaller",
                        type: "float",
                    },
                    {
                        required: true,
                        name: "bigger",
                        type: "float",
                    },
                ],
                outputs: [
                    {
                        type: 'boolean',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'All true',
                block_function: 'operator_and',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "boolean",
                    },
                    {
                        required: true,
                        type: "boolean",
                    },
                ],
                outputs: [
                    {
                        type: 'boolean',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Any true',
                block_function: 'operator_or',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "boolean",
                    },
                    {
                        required: true,
                        type: "boolean",
                    },
                ],
                outputs: [
                    {
                        type: 'boolean',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Inverse',
                block_function: 'operator_not',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: "boolean",
                    },
                ],
                outputs: [
                    {
                        type: 'boolean',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Join texts',
                block_function: 'operator_join',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: "beginning",
                        type: "string",
                    },
                    {
                        required: true,
                        name: "end",
                        type: "string",
                    },
                ],
                outputs: [
                    {
                        type: 'string',
                    },
                ]
            },
            // Advanced block
            {
                icon: PLATFORM_ICON,
                message: 'Get key %i1 of %i2',
                block_function: 'operator_json_parser',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        name: "key",
                        type: "string",
                    },
                    {
                        required: true,
                        name: "dictionary",
                        type: "any",
                    },
                ],
                outputs: [
                    {
                        type: 'any',
                    },
                ]
            },
        ]
    },
    {
        id: 'debug',
        name: 'Debug',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'Log value %i1',
                block_function: 'logging_add_log',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        type: "any",
                    },
                ],
            },
        ]
    },
    {
        id: 'time',
        name: 'Time',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'UTC date',
                block_function: 'flow_utc_date',
                type: 'getter',
                outputs: [
                    {
                        name: 'year',
                        type: 'integer',
                    },
                    {
                        name: 'month',
                        type: 'integer',
                    },
                    {
                        name: 'day',
                        type: 'integer',
                    },
                    {
                        name: 'day of week',
                        type: 'any',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'UTC time',
                block_function: 'flow_utc_time',
                type: 'getter',
                outputs: [
                    {
                        name: 'hour',
                        type: 'integer',
                    },
                    {
                        name: 'minute',
                        type: 'integer',
                    },
                    {
                        name: 'second',
                        type: 'integer',
                    },
                ]
            }
        ]
    },
    {
        id: 'variables',
        name: 'Variables',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'Get %(variable) value',
                block_function: 'data_variable',
                type: 'getter',
                outputs: [
                    {
                        type: 'any'
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Set %(variable) to %i1',
                block_function: 'data_setvariableto',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        name: 'new value',
                        type: "any",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Increment %(variable) by %i1',
                block_function: 'data_changevariableby',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        type: "float",
                    },
                ]
            }
        ]
    },
    {
        id: 'lists',
        name: 'Lists',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'Add %i1 to %(list)',
                block_function: 'data_addtolist',
                inputs: [
                    {
                        required: true,
                        type: 'any',
                    }
                ],
                type: 'operation'
            },
            {
                icon: PLATFORM_ICON,
                message: 'Delete entry # %i1 to %(list)',
                block_function: 'data_deleteoflist',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        type: 'integer',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Delete all of %(list)',
                block_function: 'data_deletealloflist',
                type: 'operation'
            },
            {
                icon: PLATFORM_ICON,
                message: 'Insert %i1 at position %i2 of %(list)',
                block_function: 'data_insertatlist',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        type: 'any',
                    },
                    {
                        required: true,
                        type: 'integer',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Replace item at position %i1 of %(list) with %i2',
                block_function: 'data_replaceitemoflist',
                type: 'operation',
                inputs: [
                    {
                        required: true,
                        type: 'integer',
                    },
                    {
                        required: true,
                        type: 'any',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Item number %i1 of %(list)',
                block_function: 'data_itemoflist',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: 'integer',
                    },
                ],
                outputs: [
                    {
                        type: 'any',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Position of item %i1 in %(list)',
                block_function: 'data_itemnumoflist',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: 'any',
                    },
                ],
                outputs: [
                    {
                        type: 'integer',
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Number of items in %(list)',
                block_function: 'data_lengthoflist',
                type: 'getter',
                outputs: [
                    {
                        type: 'integer',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Does %(list) contain %i1?',
                block_function: 'data_listcontainsitem',
                type: 'getter',
                inputs: [
                    {
                        required: true,
                        type: 'any',
                    }
                ],
                outputs: [
                    {
                        type: 'boolean',
                    }
                ]
            },
        ]
    },
    {
        id: ADVANCED_CATEGORY,
        name: 'Advanced',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'Get thread ID',
                block_function: 'flow_get_thread_id',
                type: 'getter',
                outputs: [
                    {
                        type: "string",
                    },
                ],
            },
            {
                icon: PLATFORM_ICON,
                message: 'When bridge %i1 connects',
                block_function: 'trigger_on_bridge_connected',
                type: 'trigger',
                inputs: [
                    {
                        required: true,
                        name: "bridge",

                        enum_name: "bridges",
                        enum_namespace:	"programaker",
                        type: "enum",
                    },
                ],
                outputs: [],
            },
            {
                icon: PLATFORM_ICON,
                message: 'When bridge %i1 connection STOPS',
                block_function: 'trigger_on_bridge_disconnected',
                type: 'trigger',
                inputs: [
                    {
                        required: true,
                        name: "bridge",

                        enum_name: "bridges",
                        enum_namespace:	"programaker",
                        type: "enum",
                    },
                ],
                outputs: [],
            },
        ]
    },
    {
        id: INTERNAL_CATEGORY,
        name: 'Internal blocks', // Not to be placed manually!
        blocks: [
            OP_PRELOAD_BLOCK,
            OP_ON_BLOCK_RUN,
        ]
    }
];

const BLOCK_MAP: {[key: string]: AtomicFlowBlockOptions} = {};
let BLOCK_MAP_READY = false;
export function get_block_from_base_toolbox(func_name: string): AtomicFlowBlockOptions {
    if (!BLOCK_MAP_READY) {
        build_block_map();
    }
    return BLOCK_MAP[func_name];
}

function build_block_map() {
    for (const cat of BaseToolboxDescription) {
        for (const block of cat.blocks) {
            let ablock = block as AtomicFlowBlockOptions;
            if (ablock.block_function) {
                BLOCK_MAP[ablock.block_function] = ablock;
            }
        }
    }
}
