import { UiSignalService } from '../../services/ui-signal.service';
import { BlockManager } from '../block_manager';
import { Area2D, BlockContextAction, Direction2D, FlowBlock, FlowBlockData, FlowBlockInitOpts, FlowBlockOptions, InputPortDefinition, Position2D } from '../flow_block';
import { FlowWorkspace } from '../flow_workspace';
import { Toolbox } from '../toolbox';
import { uuidv4 } from '../utils';
import { CutTree, UiElementWidgetType, UiElementRepr } from './renderers/ui_tree_repr';

const SvgNS = "http://www.w3.org/2000/svg";

export type UiFlowBlockType = 'ui_flow_block';
export const BLOCK_TYPE = 'ui_flow_block';

const INPUT_PORT_REAL_SIZE = 10;
const OUTPUT_PORT_REAL_SIZE = 10;
const CONNECTOR_SIDE_SIZE = 15;
const ICON_PADDING = '1ex';

export interface UiFlowBlockBuilderInitOps {
    workspace?: FlowWorkspace,
}

export type UiFlowBlockBuilder = (canvas: SVGElement,
                                  group: SVGElement,
                                  block: UiFlowBlock,
                                  service: UiSignalService,
                                  ops: UiFlowBlockBuilderInitOps) => UiFlowBlockHandler;
export interface UiFlowBlockHandler {
    onConnectionLost: (portIndex: number) => void;
    onConnectionValueUpdate : (input_index: number, value: string) => void;
    onClick: () => void,
    onInputUpdated: (connectedBlock: FlowBlock, inputIndex: number) => void,
    dispose: () => void,
    isTextEditable(): this is TextEditable,
}

export interface TextEditable {
    text: string,
    getArea(): Area2D,
};

export type OnUiFlowBlockClick = (block: UiFlowBlock, service: UiSignalService) => void;
export type OnUiFlowBlockInputUpdated = (block: UiFlowBlock, service: UiSignalService, connectedBlock: FlowBlock, inputIndex: number) => void;
export type OnDispose = () => void;

export interface UiFlowBlockOptions extends FlowBlockOptions {
    builder: UiFlowBlockBuilder,
    type: UiFlowBlockType,
    icon?: string,
    id: UiElementWidgetType,
    block_id?: string,
}

interface UiFlowBlockExtraData {
    textContent?: string,
    dimensions?: { width: number, height: number },
}

export interface UiFlowBlockData extends FlowBlockData {
    type: UiFlowBlockType,
    value: {
        options: UiFlowBlockOptions,
        extra: UiFlowBlockExtraData,
    },
}


export function isUiFlowBlockOptions(opt: FlowBlockOptions): opt is UiFlowBlockOptions {
    return ((opt as UiFlowBlockOptions).type === BLOCK_TYPE);
}

export function isUiFlowBlockData(opt: FlowBlockData): opt is UiFlowBlockData {
    return opt.type === BLOCK_TYPE;
}

export class UiFlowBlock implements FlowBlock {
    private canvas: SVGElement;
    options: UiFlowBlockOptions;

    private group: SVGElement;
    private position: {x: number, y: number};
    private output_groups: SVGGElement[];
    private input_groups: SVGGElement[];
    private blockId: string;
    private input_count: number[] = [];
    protected handler: UiFlowBlockHandler;
    private input_blocks: [FlowBlock, number][] = [];
    private workspace: FlowWorkspace | null;

    blockData: UiFlowBlockExtraData = {};

    constructor(options: UiFlowBlockOptions,
                private uiSignalService: UiSignalService,
               ) {
        this.options = options;
        if (!this.options.outputs) {
            this.options.outputs = [];
        }
        if (!this.options.inputs) {
            this.options.inputs = [];
        }
        this.output_groups = [];
        this.input_groups = [];
    }

    public dispose() {
        this.canvas.removeChild(this.group);

        this.handler.dispose();
    }

    public get id() {
        return this.blockId;
    }

    public render(canvas: SVGElement, initOpts: FlowBlockInitOpts): SVGElement {
        if (this.group) { return this.group } // Avoid double initialization
        this.workspace = initOpts.workspace;

        this.canvas = canvas;
        if (initOpts.position) {
            this.position = { x: initOpts.position.x, y: initOpts.position.y };
        }
        else {
            if (this.options.inputs && this.options.inputs.length > 0) {
                this.position = {x: 0, y: INPUT_PORT_REAL_SIZE};
            }
            else {
                this.position = {x: 0, y: 0};
            }
        }

        if (initOpts.block_id) {
            this.blockId = initOpts.block_id;
        }
        else {
            this.blockId = uuidv4();
        }

        const group = document.createElementNS(SvgNS, 'g');
        this.canvas.appendChild(group);

        this.handler = this.options.builder(canvas, group, this, this.uiSignalService, {
            workspace: initOpts.workspace,
        });
        this._renderOutputs(group);
        this._renderInputs(group);

        this.group = group;
        this.group.onclick = this.onclick.bind(this);

        this.moveBy({x: 0, y: 0}); // Apply transformation

        return this.group;
    }

    public renderAsUiElement(): CutTree {
        const data: UiElementRepr = { id: this.blockId, widget_type: this.options.id };

        if (this.blockData.textContent) {
            data.text = this.blockData.textContent;
        }

        return data;
    }

    private _renderOutputs(group: SVGGElement) {
        const nodeBox = group.getClientRects()[0];

        // Add outputs
        let output_index = -1;

        const output_initial_x_position = 10; // px
        const outputs_x_margin = 10; // px
        const output_plating_x_margin = 3; // px

        let output_x_position = output_initial_x_position;

        for (const output of this.options.outputs) {
            output_index++;

            const out_group = document.createElementNS(SvgNS, 'g');
            out_group.classList.add('output');
            group.appendChild(out_group);

            const port_external = document.createElementNS(SvgNS, 'circle');
            const port_internal = document.createElementNS(SvgNS, 'circle');

            out_group.appendChild(port_external);
            out_group.appendChild(port_internal);

            const output_port_size = 50;
            const output_port_internal_size = 5;
            const output_position_start = output_x_position;
            let output_position_end = output_x_position + output_port_size;


            if (output.name) {
                // Bind output name and port
                const port_plating = document.createElementNS(SvgNS, 'rect');
                out_group.appendChild(port_plating);

                const text = document.createElementNS(SvgNS, 'text');
                text.textContent = output.name;
                text.setAttributeNS(null, 'class', 'argument_name output');
                out_group.appendChild(text);

                output_position_end = Math.max(output_position_end, (output_x_position
                                                                     + text.getClientRects()[0].width
                                                                     + output_plating_x_margin * 2));

                const output_width = output_position_end - output_position_start;

                text.setAttributeNS(null, 'x', output_position_start + output_width/2 - text.getClientRects()[0].width/2  + '');
                text.setAttributeNS(null, 'y', nodeBox.height - (OUTPUT_PORT_REAL_SIZE/2) + '' );

                output_x_position = output_position_end + outputs_x_margin;

                const output_height = Math.max(output_port_size / 2, (OUTPUT_PORT_REAL_SIZE
                                                                      + text.getClientRects()[0].height));

                // Configure port connector now that we know where the output will be positioned
                port_plating.setAttributeNS(null, 'class', 'port_plating');
                port_plating.setAttributeNS(null, 'x', output_position_start + '');
                port_plating.setAttributeNS(null, 'y', nodeBox.height - output_height/1.5 + '');
                port_plating.setAttributeNS(null, 'width', (output_position_end - output_position_start) + '');
                port_plating.setAttributeNS(null, 'height', output_height/1.5 + '');

            }
            else {
                output_x_position += output_port_size;
            }

            let type_class = 'unknown_type';
            if (output.type) {
                type_class = output.type + '_port';
            }

            // Draw the output port
            const port_x_center = (output_position_start + output_position_end) / 2;
            const port_y_center = nodeBox.height;

            port_external.setAttributeNS(null, 'class', 'output external_port ' + type_class);
            port_external.setAttributeNS(null, 'cx', port_x_center + '');
            port_external.setAttributeNS(null, 'cy', port_y_center + '');
            port_external.setAttributeNS(null, 'r', OUTPUT_PORT_REAL_SIZE + '');

            port_internal.setAttributeNS(null, 'class', 'output internal_port');
            port_internal.setAttributeNS(null, 'cx', port_x_center + '');
            port_internal.setAttributeNS(null, 'cy', port_y_center + '');
            port_internal.setAttributeNS(null, 'r', output_port_internal_size + '');

            if (this.options.on_io_selected) {
                const element_index = output_index; // Capture for use in callback
                out_group.onclick = ((_ev: MouseEvent) => {
                    this.options.on_io_selected(this, 'out', element_index, output,
                                                { x: port_x_center, y: port_y_center });
                });
            }
            this.output_groups[output_index] = out_group;
        }
    }

    private _renderInputs(group: SVGGElement) {
        const nodeBox = group.getClientRects()[0];

        // Add inputs
        let input_index = -1;

        const input_initial_x_position = 10; // px
        const inputs_x_margin = 10; // px
        const input_plating_x_margin = 3; // px

        let input_x_position = input_initial_x_position;

        for (const input of this.options.inputs) {
            input_index++;

            const in_group = document.createElementNS(SvgNS, 'g');
            in_group.classList.add('input');
            group.appendChild(in_group);

            const port_external = document.createElementNS(SvgNS, 'circle');
            const port_internal = document.createElementNS(SvgNS, 'circle');

            in_group.appendChild(port_external);
            in_group.appendChild(port_internal);

            const input_port_size = 50;
            const input_port_internal_size = 5;
            const input_position_start = input_x_position;
            let input_position_end = input_x_position + input_port_size;


            if (input.name) {
                // Bind input name and port
                const port_plating = document.createElementNS(SvgNS, 'rect');
                in_group.appendChild(port_plating);

                const text = document.createElementNS(SvgNS, 'text');
                text.textContent = input.name;
                text.setAttributeNS(null, 'class', 'argument_name input');
                in_group.appendChild(text);

                input_position_end = Math.max(input_position_end, (input_x_position
                                                                     + text.getClientRects()[0].width
                                                                     + input_plating_x_margin * 2));

                const input_width = input_position_end - input_position_start;

                text.setAttributeNS(null, 'x', input_position_start + input_width/2 - text.getClientRects()[0].width/2  + '');
                text.setAttributeNS(null, 'y', nodeBox.height - (INPUT_PORT_REAL_SIZE/2) + '' );

                input_x_position = input_position_end + inputs_x_margin;

                const input_height = Math.max(input_port_size / 2, (INPUT_PORT_REAL_SIZE
                                                                      + text.getClientRects()[0].height));

                // Configure port connector now that we know where the input will be positioned
                port_plating.setAttributeNS(null, 'class', 'port_plating');
                port_plating.setAttributeNS(null, 'x', input_position_start + '');
                port_plating.setAttributeNS(null, 'y', nodeBox.height - input_height/1.5 + '');
                port_plating.setAttributeNS(null, 'width', (input_position_end - input_position_start) + '');
                port_plating.setAttributeNS(null, 'height', input_height/1.5 + '');

            }
            else {
                input_x_position += input_port_size;
            }

            let type_class = 'unknown_type';
            if (input.type) {
                type_class = input.type + '_port';
            }

            // Draw the input port
            const port_x_center = (input_position_start + input_position_end) / 2;
            const port_y_center = 0;

            port_external.setAttributeNS(null, 'class', 'input external_port ' + type_class);
            port_external.setAttributeNS(null, 'cx', port_x_center + '');
            port_external.setAttributeNS(null, 'cy', port_y_center + '');
            port_external.setAttributeNS(null, 'r', INPUT_PORT_REAL_SIZE + '');

            port_internal.setAttributeNS(null, 'class', 'input internal_port');
            port_internal.setAttributeNS(null, 'cx', port_x_center + '');
            port_internal.setAttributeNS(null, 'cy', port_y_center + '');
            port_internal.setAttributeNS(null, 'r', input_port_internal_size + '');

            if (this.options.on_io_selected) {
                const element_index = input_index; // Capture for use in callback
                in_group.onclick = ((_ev: MouseEvent) => {
                    this.options.on_io_selected(this, 'in', element_index, input,
                                                { x: port_x_center, y: port_y_center });
                });
            }
            this.input_groups[input_index] = in_group;
        }
    }

    public static GetBlockType(): string {
        return BLOCK_TYPE;
    }

    serialize(): FlowBlockData {
        return {
            type: BLOCK_TYPE,
            value: {
                options: JSON.parse(JSON.stringify(this.options)),
                extra: JSON.parse(JSON.stringify(this.blockData)),
            },
        }
    }

    public static Deserialize(data: UiFlowBlockData, manager: BlockManager, toolbox: Toolbox): FlowBlock {
        if (data.type !== BLOCK_TYPE){
            throw new Error(`Block type mismatch, expected ${BLOCK_TYPE} found: ${data.type}`);
        }

        const options: UiFlowBlockOptions = JSON.parse(JSON.stringify(data.value.options));
        options.on_dropdown_extended = manager.onDropdownExtended.bind(manager);
        options.on_inputs_changed = manager.onInputsChanged.bind(manager);
        options.on_io_selected = manager.onIoSelected.bind(manager);

        const templateOptions = this._findTemplateOptions(options.id, toolbox);
        options.builder = templateOptions.builder;

        const block = new UiFlowBlock(options, toolbox.uiSignalService);
        block.blockData = JSON.parse(JSON.stringify(data.value.extra));

        return block;
    }

    protected static _findTemplateOptions(blockId: string, toolbox: Toolbox): UiFlowBlockOptions {
        for (const block of toolbox.blocks) {
            if ((block as UiFlowBlockOptions).id === blockId) {
                // This is done in this order to detect the cases where the ID
                // matches but the type doesn't (the `else`)
                if (isUiFlowBlockOptions(block)) {
                    return block;
                }
                else {
                    throw new Error(`BlockId found with different type (type: ${(block as any).type}).`);
                }
            }
        }

        throw new Error(`Renderer not found for block (id: ${blockId}).`);
    }

    public getBodyElement(): SVGElement {
        return this.group;
    }

    public getBodyArea(): Area2D {
        const rect = (this.group as any).getBBox();
        return {
            x: this.position.x,
            y: this.position.y,
            width: rect.width,
            height: rect.height,
        }
    }

    public getOffset(): Position2D {
        return {x: this.position.x, y: this.position.y};
    }

    public moveBy(distance: {x: number, y: number}) {
        if (!this.group) {
            throw Error("Not rendered");
        }

        this.position.x += distance.x;
        this.position.y += distance.y;
        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)
    }

    addConnection(direction: "in" | "out", input_index: number, block: FlowBlock): void {
        if (direction === 'out') { return; }

        this.input_blocks.push([block, input_index]);
        this.handler.onInputUpdated(block, input_index);

        if (!this.input_count[input_index]) {
            this.input_count[input_index] = 0;
        }
        this.input_count[input_index]++;

        const extra_opts = this.options.extra_inputs;
        if (!extra_opts) {
            return;
        }

        // Consider need for extra inputs
        let has_available_inputs = false;
        for (let i = 0; i < this.options.inputs.length; i++) {
            if (!this.input_count[i]) {
                has_available_inputs = true;
                break;
            }
        }

        if (has_available_inputs) {
            // Available inputs, nothing to do
            return;
        }


        if ((extra_opts.quantity === 'any')
            || (extra_opts.quantity.max < this.input_groups.length)) {

            throw new Error("Generation of extra inputs not implemented.");
        }
    }

    removeConnection(direction: "in" | "out", portIndex: number, block: FlowBlock): void {
        if (direction === 'out') { return; }

        if (this.input_count[portIndex]) {
            this.input_count[portIndex]--;
        }

        const index = this.input_blocks.findIndex(([x, y]) => x === block);

        this.input_blocks.splice(index, 1);
        this.handler.onConnectionLost(portIndex);
    }

    updateConnectionValue(block: FlowBlock, value: string) {
        const input = this.input_blocks.find(([x, y]) => x === block);
        let input_index = -1;
        if (input) {
            input_index = input[1];
        }

        this.handler.onConnectionValueUpdate(input_index, value);
    }

    public getBlockContextActions(): BlockContextAction[] {
        const actions: BlockContextAction[] = [];

        const handler = this.handler;
        if (handler.isTextEditable()) {
            actions.push({
                title: 'Edit text',
                run: () => {
                    const prevValue = handler.text;

                    const area = handler.getArea();
                    const offset = this.getOffset();

                    area.x += offset.x;
                    area.y += offset.y;
                    this.group.classList.add('editing');

                    this.workspace.editInline(area, prevValue, 'string', (newValue: string) => {
                        this.group.classList.remove('editing');

                        if (newValue.trim().length > 0) {
                            handler.text = newValue.trim();
                        }
                    });

                }
            });
        }

        return actions;
    }

    getSlots(): { [key: string]: string; } {
        return {};
    }

    getInputs(): InputPortDefinition[] {
        return JSON.parse(JSON.stringify(this.options.inputs));
    }

    getPositionOfInput(index: number, edge?: boolean): Position2D {
        const group = this.input_groups[index];
        const circle = group.getElementsByTagName('circle')[0];
        const position = { x: parseInt(circle.getAttributeNS(null, 'cx')),
                           y: parseInt(circle.getAttributeNS(null, 'cy')),
                         };

        if (edge) {
            position.y += INPUT_PORT_REAL_SIZE;
        }

        return position;
    }

    getPositionOfOutput(index: number, edge?: boolean): Position2D {
        const group = this.output_groups[index];
        const circle = group.getElementsByTagName('circle')[0];
        const position = { x: parseInt(circle.getAttributeNS(null, 'cx')),
                           y: parseInt(circle.getAttributeNS(null, 'cy')),
                         };

        if (edge) {
            position.y += OUTPUT_PORT_REAL_SIZE;
        }

        return position;
    }

    getOutputType(index: number): string {
        return this.options.outputs[index].type;
    }

    getOutputRunwayDirection(): Direction2D {
        return 'down';
    }

    // UI Flow block specific
    onclick() {
        this.handler.onClick();
    }
}
