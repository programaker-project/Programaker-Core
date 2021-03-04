import { BlockManager } from './block_manager';
import {
    Area2D, BlockContextAction, Direction2D, FlowBlock,

    FlowBlockData, FlowBlockInitOpts, InputPortDefinition,
    MessageType, OnIOSelected,
    Position2D,
    BridgeEnumInputPortDefinition,
    BridgeEnumSequenceInputPortDefinition
} from './flow_block';
import { FlowWorkspace } from './flow_workspace';
import { manageTopLevelError } from '../utils';
import { SEPARATION } from './ui-blocks/renderers/positioning';

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

export const SEQUENCE_SEPARATOR = '\\';

export type EnumValue = {
    id: string,
    name: string,
};

export type EnumGetter = (namespace: string, name: string, selector?: string) => EnumValue[] | Promise<EnumValue[]>;

export type OnSelectRequested = ((block: FlowBlock,
                                  previous_value: string,
                                  values: EnumValue[],
                                  value_dict: {[key:string]: EnumValue},
                                  update: (new_value: string) => void,
                                 ) => void);

export interface EnumDirectValueOptions {
    definition: BridgeEnumInputPortDefinition | BridgeEnumSequenceInputPortDefinition,
    get_values: EnumGetter;
    type?: MessageType,
    on_io_selected?: OnIOSelected,
    on_select_requested?: OnSelectRequested,
};

export function isEnumDirectValueBlockData(opt: FlowBlockData): opt is EnumDirectValueFlowBlockData {
    return opt.type === BLOCK_TYPE;
}

function tagDepth(parent: string, list: EnumValue[]): EnumValue[] {
    const newValues = [] as EnumValue[];
    for (const item of list) {
        newValues.push({
            name: item.name,
            id: `${parent}${SEQUENCE_SEPARATOR}${item.id}`,
        });
    }

    return newValues;
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
    private group: SVGGElement;
    private node: SVGGElement;
    private rect: SVGRectElement;
    private rectShadow: SVGRectElement;
    private textBox: SVGTextElement;
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

        // Port over from past structure
        if (!options.definition) {
            options.definition = {
                type: 'enum',
                enum_name: (options as any).enum_name,
                enum_namespace: (options as any).enum_namespace,
            }
            delete (options as any).enum_name;
            delete (options as any).enum_namespace;
        }

        options.on_io_selected = manager.onIoSelected.bind(manager);
        options.on_select_requested = manager.onSelectRequested.bind(manager);
        options.get_values = enumGetter;

        const block = new EnumDirectValue(options, blockId);

        block.value_id = data.value.value_id;
        block._defaultText = data.value.value_text;

        return block;
    }

    public getBodyElement(): SVGGraphicsElement {
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
            this.textBox.textContent = (selected && selected.name) || '-';
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

    static cleanSequenceValue(value: string): string {
        const chunks = value.split(SEQUENCE_SEPARATOR);
        return chunks[chunks.length - 1];
    }

    private loadValues() {

            const selectValue = (id: string) => {
                this.group.classList.remove('editing');
                const oldValue = this.value_id;
                this.setValue(id);

                if (this.options.definition.type === 'enum') {
                    return;
                }
                else if (this.options.definition.type !== 'enum_sequence') {
                    throw Error(`Unknown enum type: ${(this.options.definition as any).type}`);
                }

                if (id === 'Select') {
                    on_done([{ id: "Select", name: 'Not found' }])
                    return;
                }

                if ((id === oldValue) && (this.values)) {
                    console.debug("Skipping reload on", oldValue, '->', id);
                    return;
                }


                const chunks = id ? id.split(SEQUENCE_SEPARATOR) : [];

                const foundName = this.values ? this.values.find((value: EnumValue) => value.id === id) : null;
                let selectedName = foundName ? foundName.name : null;
                if (selectedName && selectedName.match(/Go back [0-9]+ steps?/)) {
                    // TODO: Properly extract this name
                    selectedName = null;
                }

                const selectedDepth = chunks.length;

                if (selectedDepth === 0) {
                    this.setValue('Select');
                    const result = this.options.get_values(this.options.definition.enum_namespace, this.options.definition.enum_sequence[0]);

                    if ((result as any).then) {
                        (result as Promise<EnumValue[]>).then(on_done);
                    }
                    else {
                        on_done(result as EnumValue[]);
                    }

                    return;
                }
                else {
                    const depth = selectedDepth < this.options.definition.enum_sequence.length
                        ? selectedDepth
                        : this.options.definition.enum_sequence.length - 1;

                    let _fullRef: string[];
                    if (depth === selectedDepth) {
                        _fullRef = chunks;
                    }
                    else {
                        _fullRef = chunks.slice(0, chunks.length - 1); // Parent
                    }
                    const fullReference = _fullRef.join(SEQUENCE_SEPARATOR);
                    const lastLevel = _fullRef[_fullRef.length - 1];

                    const result = this.options.get_values(this.options.definition.enum_namespace,
                                                           this.options.definition.enum_sequence[depth],
                                                           lastLevel);

                    const loopNext = manageTopLevelError((values: EnumValue[]) => {

                        const prelude : EnumValue[] = [ { name: "Back to Top", id: '' } ];
                        for (let i = 1; i < depth; i++ ) {
                            prelude.push({
                                name: `Go back ${i} step` + (i === 1 ? '' : 's'),
                                id: id.split(SEQUENCE_SEPARATOR, depth - i).join(SEQUENCE_SEPARATOR),
                            });
                        }

                        const newName = selectedName ? `Select in ${selectedName}` : 'Select';
                        if (depth === selectedDepth) {
                            // This is not needed if we're "seeing" it from another level
                            prelude.push({ name: newName, id: id });
                        }

                        const menu = prelude.concat(tagDepth(fullReference, values));

                        on_done(menu);
                        this.setValue(id);
                    });

                    if ((result as any).then) {
                        (result as Promise<EnumValue[]>).then(loopNext);
                    }
                    else {
                        loopNext(result as EnumValue[]);
                    }
                }
            };

        const initialize = () => {
            const startEditing = manageTopLevelError(() => {
                if (this.options.on_select_requested) {
                    this.group.classList.add('editing');

                    this.options.on_select_requested(
                        this, this.value_id, this.values, this.value_dict,
                        manageTopLevelError(selectValue)
                    );
                }
            });

            this.textBox.onclick = startEditing;
            this.getBodyElement().ontouchend = startEditing;
        };

        const on_done = (values: EnumValue[]) => {
            this.values = values;

            this.value_dict = {};
            for (const value of values) {
                this.value_dict[value.id] = value;
            }

            initialize();

            if (this.value_id === undefined) {
                this.setValue(values[0].id);
            }
        }

        if (this.value_id !== undefined) {
            this.textBox.textContent = this._defaultText;
        }

        if (this.options.definition.type === 'enum') {
            const result = this.options.get_values(this.options.definition.enum_namespace, this.options.definition.enum_name);
            if ((result as any).then) {
                (result as Promise<EnumValue[]>).then(on_done);
            }
            else {
                on_done(result as EnumValue[]);
            }
        }
        else if (this.options.definition.type === 'enum_sequence') {
            selectValue(this.value_id || '');
        }
        else {
            throw Error(`Unknown enum type: ${(this.options.definition as any).type}`);
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
            x: -(this.textBox.getBoundingClientRect().left - this.node.getBoundingClientRect().left),
            y: -(this.textBox.getBoundingClientRect().top - this.node.getBoundingClientRect().top)
        };

        const box_height = (this.textBox.getBoundingClientRect().height * 2 + y_padding * 2);

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
        widest_section = Math.max(widest_section, this.textBox.getBoundingClientRect().width + OUTPUT_PORT_SIZE);

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
