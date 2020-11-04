import { BridgeIndexData } from '../bridges/bridge';
import { BridgeService } from '../bridges/bridge.service';
import { ResolvedBlockArgument, ResolvedCustomBlock, ResolvedDynamicBlockArgument } from '../custom_block';
import { CustomBlockService } from '../custom_block.service';
import { iconDataToUrl } from '../utils';
import { AtomicFlowBlock, isAtomicFlowBlockOptions } from './atomic_flow_block';
import { InputPortDefinition, MessageType, OutputPortDefinition } from './flow_block';
import { FlowWorkspace } from './flow_workspace';
import { Toolbox } from './toolbox';
import { BaseToolboxDescription } from './base_toolbox_description';
import { EnvironmentService } from 'app/environment.service';
import { UiToolboxDescription } from './ui-blocks/ui_toolbox_description';
import { UiFlowBlock } from './ui-blocks/ui_flow_block';
import { UiSignalService } from 'app/services/ui-signal.service';


export function buildBaseToolbox(baseElement: HTMLElement,
                                 workspace: FlowWorkspace,
                                 uiSignalService: UiSignalService,
                                ): Toolbox {
    const tb = Toolbox.BuildOn(baseElement, workspace, uiSignalService);

    for (const category of [...UiToolboxDescription, ...BaseToolboxDescription]) {
        tb.setCategory({ id: category.id, name: category.name });
        for (const block of category.blocks) {
            if (isAtomicFlowBlockOptions(block)) {
                tb.addBlockGenerator((manager) => {

                    const desc = Object.assign({
                        on_io_selected: manager.onIoSelected.bind(manager),
                        on_dropdown_extended: manager.onDropdownExtended.bind(manager),
                        on_inputs_changed: manager.onInputsChanged.bind(manager),
                    }, block);

                    return new AtomicFlowBlock(desc);
                }, category.id);
            }
            else {
                tb.addBlock(block);

                tb.addBlockGenerator((manager) => {
                    const desc = Object.assign({
                        on_io_selected: manager.onIoSelected.bind(manager),
                        on_dropdown_extended: manager.onDropdownExtended.bind(manager),
                        on_inputs_changed: manager.onInputsChanged.bind(manager),
                    }, block);

                    return new UiFlowBlock(desc, uiSignalService);
                }, category.id);
            }
        }
    }

    return tb;
}

export async function fromCustomBlockService(baseElement: HTMLElement,
                                             workspace: FlowWorkspace,
                                             customBlockService: CustomBlockService,
                                             bridgeService: BridgeService,
                                             environmentService: EnvironmentService,
                                             programId: string,
                                             uiSignalService: UiSignalService,
                                            ): Promise<Toolbox> {
    const base = buildBaseToolbox(baseElement, workspace, uiSignalService);

    const data = await bridgeService.listUserBridges();

    const bridges = data.bridges;
    const bridge_by_id: {[key: string]: BridgeIndexData} = {} ;

    for (const bridge of bridges) {
        bridge_by_id[bridge.id] = bridge;
        base.setCategory({ id: bridge.id, name: bridge.name });
    }

    const skip_resolve_argument_options = true; // Enum options will be filled when needed
    const blocks = await customBlockService.getCustomBlocksOnProgram(programId, skip_resolve_argument_options);
    for (const block of blocks) {
        let icon = null;

        const bridge = bridge_by_id[block.service_port_id];
        if (bridge) {
            icon = iconDataToUrl(environmentService, bridge.icon, bridge.id);
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
        if ((block.save_to as any) === 'undefined') {
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
    if ((block.save_to as any) === 'undefined') {
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
