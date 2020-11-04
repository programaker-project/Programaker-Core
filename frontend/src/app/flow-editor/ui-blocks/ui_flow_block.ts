import { Area2D, Direction2D, FlowBlock, FlowBlockOptions, InputPortDefinition, Position2D, FlowBlockData, FlowBlockInitOpts } from '../flow_block';
import { BlockManager } from '../block_manager';
import { Toolbox } from '../toolbox';
import { UiSignalService } from 'app/services/ui-signal.service';
import { uuidv4 } from '../utils';

const SvgNS = "http://www.w3.org/2000/svg";

export type UiFlowBlockType = 'ui_flow_block';
export const BLOCK_TYPE = 'ui_flow_block';

const INPUT_PORT_REAL_SIZE = 10;
const OUTPUT_PORT_REAL_SIZE = 10;
const CONNECTOR_SIDE_SIZE = 15;
const ICON_PADDING = '1ex';

export type UiFlowBlockRenderer = (canvas: SVGElement, group: SVGElement) => void;
export type OnUiFlowBlockClick = (block: UiFlowBlock, service: UiSignalService) => void;

export interface UiFlowBlockOptions extends FlowBlockOptions {
    renderer: UiFlowBlockRenderer;
    onclick?: OnUiFlowBlockClick;
    type: UiFlowBlockType;
    icon?: string,
    id: string,
    block_id?: string,
}

export interface UiFlowBlockData extends FlowBlockData {
    type: UiFlowBlockType,
    value: {
        options: UiFlowBlockOptions,
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
    private blockId: string;

    constructor(options: UiFlowBlockOptions,
                private uiSignalService: UiSignalService,
               ) {
        this.options = options;
        this.output_groups = [];
    }

    public dispose() {
        this.canvas.removeChild(this.group);
    }

    public get id() {
        return this.blockId;
    }

    public render(canvas: SVGElement, initOpts: FlowBlockInitOpts): SVGElement {
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

        if (this.group) { return this.group }
        const group = document.createElementNS(SvgNS, 'g');
        this.canvas.appendChild(group);

        this.options.renderer(canvas, group);
        this._renderOutputs(group);

        this.group = group;
        this.group.onclick = this.onclick.bind(this);

        this.moveBy({x: 0, y: 0}); // Apply transformation

        return this.group;
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

    public static GetBlockType(): string {
        return BLOCK_TYPE;
    }

    serialize(): FlowBlockData {
        return {
            type: BLOCK_TYPE,
            value: {
                options: JSON.parse(JSON.stringify(this.options)),
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
        options.renderer = templateOptions.renderer;
        options.onclick = templateOptions.onclick;

        return new UiFlowBlock(options, toolbox.uiSignalService);
    }

    private static _findTemplateOptions(blockId: string, toolbox: Toolbox): UiFlowBlockOptions {
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

    addConnection(direction: "in" | "out", input: number): void {
        if (direction === 'out') { return; }

        throw new Error("Method not implemented.");
    }

    removeConnection(direction: "in" | "out", index: number): void {
        if (direction === 'out') { return; }

        throw new Error("Method not implemented.");
    }

    getSlots(): { [key: string]: string; } {
        return {};
    }

    getInputs(): InputPortDefinition[] {
        return [];
        throw new Error("Method not implemented.");
    }

    getPositionOfInput(index: number, edge?: boolean): Position2D {
        throw new Error("Method not implemented.");
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
        if (this.options.onclick) {
            this.options.onclick(this, this.uiSignalService);
        }
        else {
            console.debug("No click handler on", this);
        }
    }
}
