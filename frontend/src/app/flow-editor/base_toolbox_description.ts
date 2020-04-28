import { PLATFORM_ICON } from './definitions';
import { AtomicFlowBlockOptions } from './atomic_flow_block';

interface Category {
    id: string,
    name: string,
    blocks: AtomicFlowBlockOptions[],
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

export const BaseToolboxDescription: ToolboxDescription = [
    {
        id: 'control',
        name: 'Control',
        blocks: [
            {
                icon: PLATFORM_ICON,
                message: 'Wait',
                block_function: 'op_wait_seconds',
                type: 'operation',
                inputs: [
                    {
                        name: "seconds to wait",
                        type: "integer",
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
                message: 'Wait for',
                block_function: 'trigger_wait_for',
                type: 'trigger',
                inputs: [
                    {
                        name: "check",
                        type: "boolean",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Wait for',
                block_function: 'trigger_on_signal',
                type: 'trigger',
                inputs: [
                    {
                        type: "any",
                    },
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Wait for next value',
                block_function: 'op_wait_for_next_value',
                type: 'operation',
                inputs: [
                    {
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
                block_function: 'op_repeat_times',
                type: 'operation',
                inputs: [
                    {
                        name: "start loop",
                        type: "pulse",
                    },
                    {
                        name: "repetition times",
                        type: "integer",
                    },
                    {
                        name: "back to loop",
                        type: "pulse",
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
                message: 'Only pass first',
                block_function: 'op_only_pass_first',
                type: 'operation',
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
                }
            },
            {
                icon: PLATFORM_ICON,
                message: 'Wait for all',
                block_function: 'op_wait_for_all',
                type: 'operation',
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
                }
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
                message: 'Do on parallel',
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
                block_function: 'flow_addition',
                type: 'getter',
                inputs: [
                    {
                        type: "float",
                    },
                    {
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
                block_function: 'flow_subtraction',
                type: 'getter',
                inputs: [
                    {
                        type: "float",
                    },
                    {
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
                block_function: 'flow_product',
                type: 'getter',
                inputs: [
                    {
                        type: "float",
                    },
                    {
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
                block_function: 'flow_division',
                type: 'getter',
                inputs: [
                    {
                        name: 'dividend',
                        type: "float",
                    },
                    {
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
                block_function: 'flow_modulo',
                type: 'getter',
                inputs: [
                    {
                        name: 'dividend',
                        type: "float",
                    },
                    {
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
                block_function: 'flow_greater_than',
                type: 'getter',
                inputs: [
                    {
                        name: "bigger",
                        type: "float",
                    },
                    {
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
                block_function: 'flow_equals',
                type: 'getter',
                inputs: [
                    {
                        type: "any",
                    },
                    {
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
                block_function: 'flow_lesser_than',
                type: 'getter',
                inputs: [
                    {
                        name: "smaller",
                        type: "float",
                    },
                    {
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
                block_function: 'flow_all_true',
                type: 'getter',
                inputs: [
                    {
                        type: "boolean",
                    },
                    {
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
                block_function: 'flow_any_true',
                type: 'getter',
                inputs: [
                    {
                        type: "boolean",
                    },
                    {
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
                block_function: 'flow_logic_not',
                type: 'getter',
                inputs: [
                    {
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
                block_function: 'flow_join_text',
                type: 'getter',
                inputs: [
                    {
                        name: "beginning",
                        type: "string",
                    },
                    {
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
                block_function: 'flow_get_key',
                type: 'getter',
                inputs: [
                    {
                        type: "string",
                    },
                    {
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
                message: 'Log value %1',
                block_function: 'op_log_value',
                type: 'operation',
                inputs: [
                    {
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
                block_function: 'flow_get_var_value',
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
                block_function: 'op_set_var_value',
                type: 'operation',
                inputs: [
                    {
                        name: 'new value',
                        type: "any",
                    },
                ],
                outputs: [
                    {
                        name: 'saved value',
                        type: 'any',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Increment %(variable) by %i1',
                block_function: 'op_inc_var_by',
                type: 'operation',
                inputs: [
                    {
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
                block_function: 'op_add_to_list',
                inputs: [
                    {
                        type: 'any',
                    }
                ],
                type: 'operation'
            },
            {
                icon: PLATFORM_ICON,
                message: 'Delete entry # %i1 to %(list)',
                block_function: 'op_delete_list_entry',
                type: 'operation',
                inputs: [
                    {
                        type: 'integer',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Delete all of %(list)',
                block_function: 'op_delete_all_list',
                type: 'operation'
            },
            {
                icon: PLATFORM_ICON,
                message: 'Insert %i1 at position %i2 of %(list)',
                block_function: 'op_insert_at_list_position',
                type: 'operation',
                inputs: [
                    {
                        type: 'any',
                    },
                    {
                        type: 'integer',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Replace item at position %i1 of %(list) with %i2',
                block_function: 'op_replace_at_list_position',
                type: 'operation',
                inputs: [
                    {
                        type: 'integer',
                    },
                    {
                        type: 'any',
                    }
                ]
            },
            {
                icon: PLATFORM_ICON,
                message: 'Item number %i1 of %(list)',
                block_function: 'flow_get_at_position',
                type: 'getter',
                inputs: [
                    {
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
                block_function: 'flow_find_index_of',
                type: 'getter',
                inputs: [
                    {
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
                block_function: 'flow_list_length',
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
                block_function: 'flow_list_contains',
                type: 'getter',
                inputs: [
                    {
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
        id: 'advanced',
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
            OP_PRELOAD_BLOCK,
        ]
    }
];
