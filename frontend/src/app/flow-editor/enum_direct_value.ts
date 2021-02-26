import { BlockManager } from './block_manager';
import {
    Area2D, BlockContextAction, Direction2D, FlowBlock,

    FlowBlockData, FlowBlockInitOpts, InputPortDefinition,
    MessageType, OnIOSelected,
    Position2D
} from './flow_block';
import { FlowWorkspace } from './flow_workspace';

const SvgNS = "http://www.w3.org/2000/svg";

export type EnumDirectValueFlowBlockDataType = 'enum_value_block';
export const BLOCK_TYPE = 'enum_value_block';

export interface EnumDirectValueFlowBlockData {
    type: EnumDirectValueFlowBlockDataType,
    value: {
        options: EnumDirectValueOptions
        value_id: string,
        value_text: string,
    },
}

const OUTPUT_PORT_REAL_SIZE = 10;
const MIN_WIDTH = 50;
const OUTPUT_PORT_SIZE = 25;

export type EnumValue = {
    id: string,
    name: string,
};

export type EnumGetter = (namespace: string, name: string) => EnumValue[] | Promise<EnumValue[]>;

export type OnSelectRequested = ((block: FlowBlock,
                                  previous_value: string,
                                  values: EnumValue[],
                                  value_dict: {[key:string]: EnumValue},
                                  update: (new_value: string) => void,
                                 ) => void);

export interface EnumDirectValueOptions {
    enum_name: string,
    enum_namespace: string,
    get_values: EnumGetter;
    type?: MessageType,
    on_io_selected?: OnIOSelected,
    on_select_requested?: OnSelectRequested,
};

export function isEnumDirectValueBlockData(opt: FlowBlockData): opt is EnumDirectValueFlowBlockData {
    return opt.type === BLOCK_TYPE;
}

export class EnumDirectValue implements FlowBlock {
    options: EnumDirectValueOptions;
    readonly id: string;
    readonly onMoveCallbacks: ((pos: Position2D) => void)[] = [];
    private _workspace: FlowWorkspace;

    value_id: string = undefined;

    values: EnumValue[];
    value_dict: {[key:string]: EnumValue};

    constructor(options: EnumDirectValueOptions, blockId: string) {
        this.options = options;
        this.id = blockId;
    }

    public dispose() {
        this.canvas.removeChild(this.group);
    }

    // Render elements
    private group: SVGElement;
    private node: SVGElement;
    private rect: SVGElement;
    private rectShadow: SVGElement;
    private textBox: SVGElement;
    private canvas: SVGElement;
    private _defaultText: string;

    private position: {x: number, y: number};
    private textCorrection: {x: number, y: number};
    private size: { width: number, height: number };

    public static GetBlockType(): string {
        return BLOCK_TYPE;
    }

    public serialize(): EnumDirectValueFlowBlockData {
        return {
            type: BLOCK_TYPE,
            value: { options: JSON.parse(JSON.stringify(this.options)),
                     value_id: this.getValue(),
                     value_text: this.textBox.textContent,
                   },
        }
    }

    static Deserialize(data: FlowBlockData, blockId: string, manager: BlockManager, enumGetter: EnumGetter): FlowBlock {
        if (data.type !== BLOCK_TYPE){
            throw new Error(`Block type mismatch, expected ${BLOCK_TYPE} found: ${data.type}`);
        }

        const options: EnumDirectValueOptions = JSON.parse(JSON.stringify(data.value.options));
        options.on_io_selected = manager.onIoSelected.bind(manager);
        options.on_select_requested = manager.onSelectRequested.bind(manager);
        options.get_values = enumGetter;

        const block = new EnumDirectValue(options, blockId);

        block.value_id = data.value.value_id;
        block._defaultText = data.value.value_text;

        return block;
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

    public moveTo(pos: Position2D) {
        this.position.x = pos.x;
        this.position.y = pos.y;

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)
    }

    public moveBy(distance: {x: number, y: number}): FlowBlock[] {
        if (!this.group) {
            throw Error("Not rendered");
        }

        this.position.x += distance.x;
        this.position.y += distance.y;
        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        for (const callback of this.onMoveCallbacks) {
            callback(this.position);
        }

        return [];
    }

    public onMove(callback: (pos: Position2D) => void) {
        this.onMoveCallbacks.push(callback);
    }

    public endMove(): FlowBlock[] {
        return [];
    }

    public onGetFocus() {}
    public onLoseFocus() {}

    public addConnection(direction: 'in' | 'out', _index: number): boolean {
        if (direction === 'in') {
            console.warn("Should NOT be possible to add a connection to a EnumDirectValue block");
        }

        return false;
    }

    public removeConnection(_direction: 'in' | 'out', _index: number) : boolean {
        return false;
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
        throw new Error("EnumDirectValue don't have any input");
    }

    public getPositionOfOutput(index: number, edge?: boolean): Position2D {
        return { x: 0, y: this.size.height / 2 };
    }

    public getOutputType(_index: number): string {
        return this.options.type || 'enum';
    }

    public getInputType(_index: number): string {
        throw Error("Direct enum values don't have inputs")
    }

    public getOutputRunwayDirection(): Direction2D {
        return 'left';
    }

    public getValue() {
        return this.value_id;
    }

    private setValue(id: string, sideChannel: boolean =false) {
        this.value_id = id;

        if (!this.value_dict) { return; }

        const selected = this.value_dict[id];

        if (this.group) {
            this.textBox.textContent = selected.name || '-';
            this.updateSize();
        }

        if (this._workspace && !sideChannel) {
            this._workspace.onBlockOptionsChanged(this);
        }
    }

    public updateOptions(blockData: FlowBlockData): void {
        const data = blockData as EnumDirectValueFlowBlockData;
        this.setValue(data.value.value_id, true);
        this.textBox.textContent = data.value.value_text;
    }

    private loadValues() {
        const on_done = (values: EnumValue[]) => {
            this.values = values;

            this.value_dict = {};
            for (const value of values) {
                this.value_dict[value.id] = value;
            }

            this.textBox.onclick = (() => {
                if (this.options.on_select_requested) {
                    this.group.classList.add('editing');

                    this.options.on_select_requested(this,
                                                     this.value_id,
                                                     this.values,
                                                     this.value_dict,
                                                     (id: string) => {
                                                         this.group.classList.remove('editing');
                                                         this.setValue(id);
                                                     }
                                                    );
                }
            });

            if (this.value_id === undefined) {
                this.setValue(values[0].id);
            }
        }

        if (this.value_id !== undefined) {
            this.textBox.textContent = this._defaultText;
        }

        const result = this.options.get_values(this.options.enum_namespace, this.options.enum_name);
        if ((result as any).then) {
            (result as Promise<EnumValue[]>).then(on_done);
        }
        else {
            on_done(result as EnumValue[]);
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
        this._workspace = initOpts.workspace;

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
        this.textBox.textContent = "Loading...";
        this.textBox.setAttributeNS(null,'textlength', '100%');

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

        try {
            this.loadValues();
        }
        catch (err) {
            console.error("Error loading enum values:", err);
        }

        this.updateSize();

        return this.group;
    }

}
