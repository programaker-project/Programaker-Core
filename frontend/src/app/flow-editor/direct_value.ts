import {
    FlowBlock,
    InputPortDefinition, OnIOSelected,
    Area2D, Direction2D, Position2D, MessageType, FlowBlockData, FlowBlockInitOpts, FlowBlockOptions, BlockContextAction,
} from './flow_block';
import { BlockManager } from './block_manager';
import { UiFlowBlock } from './ui-blocks/ui_flow_block';

const SvgNS = "http://www.w3.org/2000/svg";

export type DirectValueBlockType = 'direct_value_block';
export const BLOCK_TYPE = 'direct_value_block';

const OUTPUT_PORT_REAL_SIZE = 10;
const MIN_WIDTH = 50;
const OUTPUT_PORT_SIZE = 25;

export type OnRequestEdit = (block: DirectValue, type: MessageType, update: (value: string) => void) => void;

interface DirectValueOptions {
    type?: MessageType,
    value: string,
    on_io_selected?: OnIOSelected,
    on_request_edit?: OnRequestEdit,
};

export interface DirectValueFlowBlockData extends FlowBlockData {
    type: DirectValueBlockType,
    value: DirectValueOptions,
};

export function isDirectValueBlockData(opt: FlowBlockData): opt is DirectValueFlowBlockData {
    return opt.type === BLOCK_TYPE;
}

export class DirectValue implements FlowBlock {
    options: DirectValueOptions;
    value: string;
    sinks: FlowBlock[] = [];

    constructor(options: DirectValueOptions) {
        this.options = options;

        this.value = options.value;
        if (!this.value) {
            this.value = DirectValue.getDefaultValueForType(this.options.type);
        }
    }

    public dispose() {
        this.canvas.removeChild(this.group);
    }

    private static getDefaultValueForType( type?: MessageType ) {
        if (!type) { return 'sample value'; }

        switch (type) {
            case 'float':
                return '0.5';
            case 'integer':
                return '9999';
            case 'boolean':
                return 'true';

            case 'string':
                return 'sample value';

            case 'pulse':
                console.warn('TODO: Implement pulse sender');
            case 'any':
                return 'sample value';
        }
    }

    // Render elements
    private group: SVGElement;
    private node: SVGElement;
    private rect: SVGElement;
    private rectShadow: SVGElement;
    private textBox: SVGElement;
    private canvas: SVGElement;

    private position: {x: number, y: number};
    private textCorrection: {x: number, y: number};
    private size: { width: number, height: number };

    public static GetBlockType(): string {
        return BLOCK_TYPE;
    }

    public serialize(): DirectValueFlowBlockData {
        const opt = JSON.parse(JSON.stringify(this.options))
        opt.value = this.value;

        return {
            type: BLOCK_TYPE,
            value: opt,
        }
    }

    static Deserialize(data: FlowBlockData, manager: BlockManager): FlowBlock {
        if (data.type !== BLOCK_TYPE){
            throw new Error(`Block type mismatch, expected ${BLOCK_TYPE} found: ${data.type}`);
        }

        const options: DirectValueOptions = JSON.parse(JSON.stringify(data.value));
        options.on_io_selected = manager.onIoSelected.bind(manager);
        options.on_request_edit = manager.onRequestEdit.bind(manager);

        return new DirectValue(options);
    }

    public getBodyElement(): SVGElement {
        if (!this.group) {
            throw Error("Not rendered");
        }

        return this.node;
    }

    getBodyArea(): Area2D {
        const rect = (this.group as any).getBBox();
        return {
            x: this.position.x,
            y: this.position.y,
            width: rect.width,
            height: rect.height,
        }
    }

    getValueArea(): Area2D {
        const body = this.getBodyArea();

        body.x += OUTPUT_PORT_SIZE / 2;
        body.width -= OUTPUT_PORT_SIZE;

        return body;
    }

    public getOffset(): {x: number, y: number} {
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

    public addConnection(direction: 'in' | 'out', _index: number, block: FlowBlock) {
        if (direction === 'in') {
            console.warn("Should NOT be possible to add a connection to a DirectValue block");
            return;
        }

        this.sinks.push(block);
    }

    public removeConnection(direction: 'in' | 'out', _index: number, block: FlowBlock) {
        if (direction === 'in') {
            console.warn("Should NOT be possible to have input connections on a DirectValue block");
            return;
        }

        const index = this.sinks.findIndex(x => x === block);

        this.sinks.splice(index, 1);
    }

    public getBlockContextActions(): BlockContextAction[] {
        return [];
    }

    public getSlots(): {[key: string]: string} {
        return {};
    }

    public getInputs(): InputPortDefinition[] {
        return [];
    }

    public getPositionOfInput(index: number, edge?: boolean): Position2D {
        throw new Error("DirectValue don't have any input");
    }

    public getPositionOfOutput(index: number, edge?: boolean): Position2D {
        return { x: 0, y: this.size.height / 2 };
    }

    public getOutputType(_index: number): string {
        return this.options.type;
    }

    public getOutputRunwayDirection(): Direction2D {
        return 'left';
    }

    public getValue() {
        return this.value;
    }

    private setValue(new_value: string) {
        this.value = new_value;

        if (this.group) {
            this.textBox.textContent = this.value || '-';
            this.updateSize();
        }
        for (const block of this.sinks) {
            if (block instanceof UiFlowBlock) {
                block.updateConnectionValue(this, new_value);
            }
        }
    }

    private updateSize() {
        let widest_section = MIN_WIDTH;
        widest_section = Math.max(widest_section, (this.textBox as any).getBBox().width + OUTPUT_PORT_SIZE);

        const box_width = widest_section;

        // Center text box
        this.textBox.setAttributeNS(null, 'x', (this.textCorrection.x
                                                + OUTPUT_PORT_SIZE/4
                                                + box_width/2
                                                - ((this.textBox as any).getBBox().width/2)) + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");
    }

    public render(canvas: SVGElement, initOpts: FlowBlockInitOpts): SVGElement {
        if (this.group) { return this.group }

        this.canvas = canvas;
        if (initOpts.position) {
            this.position = { x: initOpts.position.x, y: initOpts.position.y };
        }
        else {
            this.position = {x: 0, y: 0};
        }

        const y_padding = 5; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');
        this.textBox = document.createElementNS(SvgNS, 'text');

        this.group.setAttribute('class', 'flow_node direct_value_node');
        this.textBox.setAttribute('class', 'node_name');
        this.textBox.textContent = "test";
        this.textBox.setAttributeNS(null,'textlength', '100%');
        this.textBox.onclick = (() => {
            if (this.options.on_request_edit) {
                this.group.classList.add('editing');

                this.options.on_request_edit(this, this.options.type || 'any',
                                             (update: string) => {
                                                 this.setValue(update);

                                                 this.group.classList.remove('editing');
                                             });
            }
        });

        this.textBox.setAttributeNS(null, 'x', "0");
        this.textBox.setAttributeNS(null, 'y', "0");

        this.node.appendChild(this.rectShadow);
        this.node.appendChild(this.rect);
        this.node.appendChild(this.textBox);
        this.group.appendChild(this.node);
        this.canvas.appendChild(this.group);

        // Read text correction
        this.textCorrection = {
            x: -(this.textBox.getClientRects()[0].left - this.node.getClientRects()[0].left),
            y: -(this.textBox.getClientRects()[0].top - this.node.getClientRects()[0].top)
        };

        this.textBox.textContent = this.value;

        const box_height = (this.textBox.getClientRects()[0].height * 2 + y_padding * 2);

        // Add direct output
        const out_group = document.createElementNS(SvgNS, 'g');
        this.group.appendChild(out_group);

        const output_port_internal_size = 5;

        let type_class = 'unknown_type';
        if (this.options.type) {
            type_class = this.options.type + '_port';
        }

        // Draw the output port
        const port_x_center = 0;
        const port_y_center = box_height / 2;

        const port_external = document.createElementNS(SvgNS, 'circle');
        port_external.setAttributeNS(null, 'class', 'output external_port ' + type_class);
        port_external.setAttributeNS(null, 'cx', port_x_center + '');
        port_external.setAttributeNS(null, 'cy', port_y_center + '');
        port_external.setAttributeNS(null, 'r', OUTPUT_PORT_REAL_SIZE + '');

        const port_internal = document.createElementNS(SvgNS, 'circle');
        port_internal.setAttributeNS(null, 'class', 'output internal_port');
        port_internal.setAttributeNS(null, 'cx', port_x_center + '');
        port_internal.setAttributeNS(null, 'cy', port_y_center + '');
        port_internal.setAttributeNS(null, 'r', output_port_internal_size + '');

        out_group.appendChild(port_external);
        out_group.appendChild(port_internal);

        if (this.options.on_io_selected) {
            out_group.onclick = ((_ev: MouseEvent) => {
                this.options.on_io_selected(this, 'out', 0, { type: this.options.type },
                                            { x: port_x_center, y: port_y_center });
            });
        }

        let widest_section = MIN_WIDTH;
        widest_section = Math.max(widest_section, this.textBox.getClientRects()[0].width + OUTPUT_PORT_SIZE);

        const box_width = widest_section;

        // Center text box
        this.textBox.setAttributeNS(null, 'x', (this.textCorrection.x
                                                + OUTPUT_PORT_SIZE/4
                                                + box_width/2
                                                - ((this.textBox as any).getBBox().width/2)) + "");
        this.textBox.setAttributeNS(null, 'y', ((this.textBox as any).getBBox().height*1.75 + this.textCorrection.y) + "");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'rx', "2px"); // Like border-radius, in px

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'rx', "2px"); // Like border-radius, in px

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        this.size = { width: box_width, height: box_height };

        return this.group;
    }

}
