import { BridgeIndexData } from '../bridges/bridge';
import { BridgeService } from '../bridges/bridge.service';
import { ResolvedBlockArgument, ResolvedCustomBlock, ResolvedDynamicBlockArgument } from '../custom_block';
import { CustomBlockService } from '../custom_block.service';
import { iconDataToUrl } from '../utils';
import { AtomicFlowBlock } from './atomic_flow_block';
import { InputPortDefinition, MessageType, OutputPortDefinition } from './flow_block';
import { FlowWorkspace } from './flow_workspace';
import { Toolbox } from './toolbox';

const PLATFORM_ICON = '/assets/logo-dark.png';

export function buildBaseToolbox(baseElement: HTMLElement, workspace: FlowWorkspace): Toolbox {
    const tb = Toolbox.BuildOn(baseElement, workspace);

    const control = 'control';

    // Control category
    tb.setCategory({ id: control, name: 'Control' })
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Wait',
            block_function: 'flow_wait_seconds',
            type: 'operation',
            inputs: [
                {
                    name: "seconds to wait",
                    type: "integer",
                },
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Check',
            block_function: 'flow_if_then',
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
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Wait for',
            block_function: 'flow_wait_for',
            type: 'trigger',
            inputs: [
                {
                    name: "check",
                    type: "boolean",
                },
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Wait for next value',
            block_function: 'flow_wait_for_next_value',
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
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Repeat times',
            block_function: 'flow_repeat_times',
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Only pass first',
            block_function: 'flow_only_pass_first',
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
            },
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Wait for all',
            block_function: 'flow_wait_for_all',
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
            },
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);


    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'When all true',
            block_function: 'flow_when_all_true',
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
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, control);

    // Operators category
    const operators = 'operators';
    tb.setCategory({ id: operators, name: 'Operators' })
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    // Advanced block
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, operators);

    // Variables
    const time = 'time';
    tb.setCategory({ id: time, name: 'Time' })
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, time);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, time);

    // Variables
    const variables = 'variables';
    tb.setCategory({ id: variables, name: 'Variables' })
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Get %(variable) value',
            block_function: 'flow_get_var_value',
            type: 'getter',
            outputs: [
                {
                    type: 'any'
                }
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, variables);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Set %(variable) to %i1',
            block_function: 'flow_set_var_value',
            type: 'operation',
            inputs: [
                {
                    name: 'new value',
                    type: "any",
                },
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, variables);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Increment %(variable) by %i1',
            block_function: 'flow_inc_var_by',
            type: 'operation',
            inputs: [
                {
                    type: "float",
                },
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, variables);


    // Lists
    const lists = 'lists';
    tb.setCategory({ id: lists, name: 'Lists' })
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Add %i1 to %(list)',
            block_function: 'flow_add_to_list',
            inputs: [
                {
                type: 'any',
                }
            ],
            type: 'operation',
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Delete entry # %i1 to %(list)',
            block_function: 'flow_delete_list_entry',
            type: 'operation',
            inputs: [
                {
                    type: 'integer',
                }
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Delete all of %(list)',
            block_function: 'flow_delete_all_list',
            type: 'operation',
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Insert %i1 at position %i2 of %(list)',
            block_function: 'flow_insert_at_list_position',
            type: 'operation',
            inputs: [
                {
                    type: 'any',
                },
                {
                    type: 'integer',
                }
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Replace item at position %i1 of %(list) with %i2',
            block_function: 'flow_replace_at_list_position',
            type: 'operation',
            inputs: [
                {
                    type: 'integer',
                },
                {
                    type: 'any',
                }
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            icon: PLATFORM_ICON,
            message: 'Number of items in %(list)',
            block_function: 'flow_list_length',
            type: 'getter',
            outputs: [
                {
                    type: 'integer',
                }
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
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
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
            on_dropdown_extended: manager.onDropdownExtended.bind(manager),
            on_inputs_changed: manager.onInputsChanged.bind(manager),
        });
    }, lists);

    return tb;
}

export async function fromCustomBlockService(baseElement: HTMLElement,
                                             workspace: FlowWorkspace,
                                             customBlockService: CustomBlockService,
                                             bridgeService: BridgeService,
                                            ): Promise<Toolbox> {
    const base = buildBaseToolbox(baseElement, workspace);

    const data = await bridgeService.listUserBridges();

    const bridges = data.bridges;
    const bridge_by_id: {[key: string]: BridgeIndexData} = {} ;

    for (const bridge of bridges) {
        bridge_by_id[bridge.id] = bridge;
        base.setCategory({ id: bridge.id, name: bridge.name });
    }

    const skip_resolve_argument_options = true; // Enum options will be filled when needed
    const blocks = await customBlockService.getCustomBlocks(skip_resolve_argument_options);
    for (const block of blocks) {
        let icon = null;

        const bridge = bridge_by_id[block.service_port_id];
        if (bridge) {
            icon = iconDataToUrl(bridge.icon, bridge.id);
        }

        base.addBlockGenerator((manager) => {
            return new AtomicFlowBlock({
                icon: icon,
                message: get_block_message(block),
                block_function: 'services.' + bridge.id + '.' + block.function_name,
                type: (block.block_type as any),
                inputs: get_block_inputs(block),
                outputs: get_block_outputs(block),
                on_io_selected: manager.onIoSelected.bind(manager),
                on_dropdown_extended: manager.onDropdownExtended.bind(manager),
                on_inputs_changed: manager.onInputsChanged.bind(manager),
            })
        }, block.service_port_id);
    }

    return base;
}

function get_output_indexes(block: ResolvedCustomBlock): number[] {
    let output_indexes = [];
    if (block.save_to) {
        if (block.save_to === 'undefined') {
            console.warn('Serialization error on block.save_to');
        }
        else if (((block.save_to as any).type !== 'argument')
            || !(((block.save_to as any).index) || ((block.save_to as any).index === 0))) {

            console.error('BLOCK save to', block);
        }
        else {
            output_indexes.push((block.save_to as any).index);
        }
    }

    return output_indexes;
}

function get_block_message(block: ResolvedCustomBlock): string {
    const output_indexes = get_output_indexes(block);

    return block.message.replace(/%(\d+)/g, (_match, digits) => {
        const num = parseInt(digits);
        if (output_indexes.indexOf(num - 1) < 0) { // %num are 1-indexed
            return `%i${digits}`;
        }
        else {
            if (output_indexes.length !== 1) {
                console.error('TODO: Index output remapping', block);
            }
            return '%o1';
        }
    });
}

function get_block_inputs(block: ResolvedCustomBlock): InputPortDefinition[] {
    // Remove save_to
    const skipped_indexes = get_output_indexes(block);

    return (block.arguments
        .filter((_value, index) => skipped_indexes.indexOf(index) < 0)
        .map((value) => (get_block_arg(block, value)) ));
}

function get_block_arg(block: ResolvedCustomBlock, arg: ResolvedBlockArgument): InputPortDefinition {
    if ((arg as ResolvedDynamicBlockArgument).callback) {
        const dyn_arg = (arg as ResolvedDynamicBlockArgument);

        return {
            type: 'enum',
            enum_name: dyn_arg.callback,
            enum_namespace: block.service_port_id,
        }
    }
    else {
        return {
            type: get_arg_type(arg),
        };
    }
}

function get_block_outputs(block: ResolvedCustomBlock): OutputPortDefinition[] {
    if (block.block_type === 'getter') {
        let result_type: MessageType = 'any';

        switch (block.block_result_type) {
            case 'string':
            case 'boolean':
            case 'integer':
            case 'float':
                result_type = block.block_result_type;
                break

            case 'number':
                break;

            case null:
                console.warn('Return type not set on', block);
                break;

            default:
                console.error("Unknown type", block.block_result_type);
        }

        return [{
            type: result_type,
        }];
    }


    // Derive from save_to
    if (!block.save_to) {
        return [];
    }
    if (block.save_to === 'undefined') {
        console.warn('Serialization error on block.save_to');
        return [];
    }

    if (((block.save_to as any).type !== 'argument')
        || !(((block.save_to as any).index) || ((block.save_to as any).index === 0))) {

        console.error('BLOCK save to', block);
    }

    const arg = block.arguments[(block.save_to as any).index];
    if (!arg) {
        console.error('BLOCK save to', block);
        return [];
    }

    return [{
        type: get_arg_type(arg),
    }];
}

function get_arg_type(arg: any): MessageType  {
    if (arg.type === 'variable') {
        return 'any';
    }

    let result_type = 'any';
    switch (arg.type) {
        case 'string':
        case 'boolean':
        case 'integer':
            result_type = arg.type;
            break

        case 'number':
        case 'float':
            break;

        case null:
            console.warn('Return type not set on', arg);
            break;

        default:
            console.error("Unknown type", arg.type);
    }

    return result_type as MessageType;
}
