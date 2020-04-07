import { BridgeIndexData } from '../bridges/bridge';
import { BridgeService } from '../bridges/bridge.service';
import { ResolvedCustomBlock } from '../custom_block';
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            message: 'Is %i1 bigger (>) than %i2 ?',
            type: 'streaming',
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
            type: 'streaming',
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
            message: 'Is %i1 smaller (<) than %i2?',
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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
            type: 'streaming',
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


    const blocks = await customBlockService.getCustomBlocks();
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
        .map((value) => ({
            type: get_arg_type(value)
        } as InputPortDefinition) ));
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
