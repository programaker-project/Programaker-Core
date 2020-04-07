import { AtomicFlowBlock } from './atomic_flow_block';
import { FlowBlock, Position2D, InputPortDefinition, MessageType, OutputPortDefinition } from './flow_block';
import { BlockExhibitor } from './block_exhibitor';
import { BlockManager } from './block_manager';
import { FlowWorkspace } from './flow_workspace';
import { CustomBlockService } from '../custom_block.service';
import { ResolvedCustomBlock } from '../custom_block';
import { BridgeService } from '../bridges/bridge.service';
import { BridgeIndexData } from '../bridges/bridge';
import { iconDataToUrl } from '../utils';

export type BlockGenerator = (manager: BlockManager) => FlowBlock;

const PLATFORM_ICON = '/assets/logo-dark.png';

export class Toolbox {
    baseElement: HTMLElement;
    toolboxDiv: HTMLDivElement;
    blockShowcase: HTMLDivElement;
    workspace: FlowWorkspace;
    categories: { [key: string]: HTMLDivElement } = {};

    public static BuildOn(baseElement: HTMLElement, workspace: FlowWorkspace): Toolbox {
        let toolbox: Toolbox;
        try {
            toolbox = new Toolbox(baseElement, workspace);
            toolbox.init();
        }
        catch(err) {
            toolbox.dispose();

            throw err;
        }

        return toolbox;
    }


    private constructor(baseElement: HTMLElement, workspace: FlowWorkspace) {
        this.baseElement = baseElement;
        this.workspace = workspace;
    }

    onResize() {}

    dispose() {
        this.baseElement.removeChild(this.toolboxDiv);
    }

    init() {
        this.toolboxDiv = document.createElement('div');
        this.toolboxDiv.setAttribute('class', 'toolbox');
        this.baseElement.appendChild(this.toolboxDiv);

        this.blockShowcase = document.createElement('div');
        this.blockShowcase.setAttribute('class', 'showcase');
        this.toolboxDiv.appendChild(this.blockShowcase);
    }

    setCategory(cat:{ id: string, name: string }) {
        const [div, updated] = this.getOrCreateCategory(cat);
        if (!updated) {
            const title = div.getElementsByClassName('category_title')[0] as HTMLDivElement;
            title.innerText = cat.name;
        }
    }

    private getOrCreateCategory(cat:{ id: string, name: string }): [HTMLDivElement, boolean] {
        let category_div = this.categories[cat.id];
        let created_now = false;

        if (!category_div) {
            category_div = this.categories[cat.id] = document.createElement('div');
            category_div.setAttribute('class', 'category empty cat_name_' + cat.name + ' cat_id_' + cat.id);
            this.blockShowcase.appendChild(category_div);

            const cat_title = document.createElement('div');
            cat_title.setAttribute('class', 'category_title');
            cat_title.innerText = cat.name;
            category_div.appendChild(cat_title)
            cat_title.onclick = () => {
                if (category_div.classList.contains('collapsed')) {
                    category_div.classList.remove('collapsed');
                }
                else {
                    category_div.classList.add('collapsed');
                }
            };

            created_now = true;
        }

        return [category_div, created_now];
    }

    addBlockGenerator(generator: BlockGenerator, category_id: string) {
        const [category_div] = this.getOrCreateCategory({ id: category_id, name: category_id })
        category_div.classList.remove('empty');

        const block_exhibitor = BlockExhibitor.FromGenerator(generator, category_div);
        const element = block_exhibitor.getElement();
        element.onmousedown = (ev: MouseEvent) => {
            try {
                const rect = block_exhibitor.getInnerElementRect();

                if (!rect) {
                    // Hidden block, ignore
                    return;
                }

                const block = generator(this.workspace);
                element.classList.add('hidden');
                this.toolboxDiv.classList.add('subsumed');

                const block_id = this.workspace.drawAbsolute(block, rect);
                (this.workspace as any)._mouseDownOnBlock(ev, block, (ev: MouseEvent) => {

                    element.classList.remove('hidden');
                    this.toolboxDiv.classList.remove('subsumed');

                    // Check if the block was dropped on the toolbox, if so remove it
                    const toolboxRect = this.toolboxDiv.getClientRects()[0];
                    if ((ev.x >= toolboxRect.x) && (ev.x <= toolboxRect.x + toolboxRect.width)) {
                        if ((ev.y >= toolboxRect.y) && (ev.y <= toolboxRect.y + toolboxRect.height)) {
                            // Dropped on toolbox
                            console.log("Dropped on toolbox, cleaning up");
                            this.workspace.removeBlock(block_id);
                        }
                    }

                });
            }
            catch (err) {
                console.error(err);
            }
        };
    }
}

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
            message: 'Is %i1 equal (=) to %i2?',
            type: 'streaming',
            inputs: [
                {
                    type: "any",
                },
                {
                    type: "any",
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
                result_type = block.block_result_type;
                break

            case 'number':
            case 'float':
                // TODO Not supported yet
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
            // TODO Not supported yet
            break;

        case null:
            console.warn('Return type not set on', arg);
            break;

        default:
            console.error("Unknown type", arg.type);
    }

    return result_type as MessageType;
}
