import { BlockManager } from './block_manager';
import { Area2D, Direction2D, FlowBlock, FlowBlockOptions, InputPortDefinition, OutputPortDefinition, Position2D, FlowBlockData, FlowBlockInitOpts, BlockContextAction } from './flow_block';
import { is_pulse } from './graph_transformations';
import { FlowConnectionData, setConnectionType } from './flow_connection';
import { EventEmitter } from 'events';
import { FlowWorkspace } from './flow_workspace';

const SvgNS = "http://www.w3.org/2000/svg";

export type AtomicFlowBlockType = 'simple_flow_block';
export const BLOCK_TYPE = 'simple_flow_block';

const INPUT_PORT_REAL_SIZE = 10;
const OUTPUT_PORT_REAL_SIZE = 10;
const CONNECTOR_SIDE_SIZE = 15;
const ICON_PADDING = '1ex';


export type AtomicFlowBlockOperationType = 'operation' | 'getter' | 'trigger';

export interface AtomicFlowBlockOptions extends FlowBlockOptions {
    type: AtomicFlowBlockOperationType;
    icon?: string,
    block_function: string,
    message: string;
    key?: string;
    subkey?: { "type": "argument", "index": number };
    fixed_pulses?: boolean;
}

export interface AtomicFlowBlockData extends FlowBlockData {
    type: AtomicFlowBlockType,
    value: {
        options: AtomicFlowBlockOptions,
        slots: {[key: string]: string},

        // This is used to indicate if a result of this block is used on another
        // flow, and thus, if a signal reporting the result of this block has to be sent.
        report_state?: boolean,

        // These counts are needed to keep the consistency when linking
        // inline arguments to it's ports
        synthetic_input_count: number,
        synthetic_output_count: number,
    },
}

type NamedVarMessageChunk = { type: 'named_var', val: string, name: string };
type MessageChunk = ( { type: 'const', val: string }
    | NamedVarMessageChunk
    | { type: 'index_var', index: number, direction: 'in' | 'out' }
                    );

function is_digit(char: string): boolean {
    switch (char) {
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            return true;

        default:
            return false;
    }
}

export function isAtomicFlowBlockOptions(opt: FlowBlockOptions): opt is AtomicFlowBlockOptions {
    return ((opt as AtomicFlowBlockOptions).type === 'operation'
        ||  (opt as AtomicFlowBlockOptions).type === 'getter'
        ||  (opt as AtomicFlowBlockOptions).type === 'trigger');
}

export function isAtomicFlowBlockData(opt: FlowBlockData): opt is AtomicFlowBlockData {
    return opt.type === BLOCK_TYPE;
}

function parse_chunks(message: string): MessageChunk[] {
    const result: MessageChunk[] = [];

    let currentChunk = [];
    let currentChunkType: 'text' | 'named_var'  | 'index_var' = 'text';
    let currentDirection: 'in' | 'out' = null;

    for (let index=0; index < message.length; index++) {
        if (currentChunkType === 'text') {
            if (message[index] != '%') {
                currentChunk.push(message[index]);
            }
            else {
                if (((index + 1) >= message.length) || ('(io'.indexOf(message[index+1]) < 0)) {
                    // Cannot continue '%(' for named, '%i' or '%o' for indexed
                    currentChunk.push(message[index]);
                }
                else if (message[index+1] === 'i' || message[index+1] === 'o') {
                    const name = currentChunk.join('');
                    result.push({type: 'const', val: name });
                    currentChunk = [];
                    currentChunkType = 'index_var';

                    if (message[index+1] === 'i') {
                        currentDirection = 'in';
                    }
                    else {
                        currentDirection = 'out';
                    }
                    index++;
                }
                else {
                    const name = currentChunk.join('');
                    result.push({type: 'const', val: name });
                    currentChunk = [];
                    currentChunkType = 'named_var';
                    index++;
                }
            }
        }
        else if (currentChunkType === 'index_var') {
            if (is_digit(message[index])) {
                currentChunk.push(message[index]);
            }
            else {
                if (currentChunk) {
                    result.push({
                        type: 'index_var',
                        index: parseInt(currentChunk.join('')) - 1, // Account for 1-index in message
                        direction: currentDirection
                    });
                    currentChunk = [];
                    currentChunkType = 'text';
                    index--;
                }
                else {
                    throw new Error(`Unclosed indexed argument '%${currentDirection[0]}. Expected number, found ${message[index]}`);
                }
            }
        }
        else {
            if (message[index] == ')') {
                const name = currentChunk.join('');
                result.push({ type: 'named_var', val: name, name: name });
                currentChunk = [];
                currentChunkType = 'text';
            }
            else {
                currentChunk.push(message[index]);
            }
        }
    }

    if (currentChunkType === 'text') {
        if (currentChunk) {
            result.push({type: 'const', val: currentChunk.join('')});
        }
    }
    else if (currentChunkType === 'index_var'){
        if (currentChunk) {
            result.push({
                type: 'index_var',
                index: parseInt(currentChunk.join('')) - 1, // Account for 1-index in message
                direction: currentDirection
            });
        }
        else {
            throw new Error("Unclosed indexed argument '%' (expected number)");
        }
    }
    else {
        throw new Error("Unclosed tag: %(" + currentChunk.join(''));
    }

    return result;
}

export class AtomicFlowBlock implements FlowBlock {
    readonly id: string;
    readonly onMoveCallbacks: ((pos: Position2D) => void)[] = [];
    private _workspace: FlowWorkspace;

    options: AtomicFlowBlockOptions;
    overridenInputTypes: ('user-pulse' | 'pulse')[] = [];
    overridenOutputTypes: ('user-pulse' | 'pulse')[] = [];

    synthetic_input_count = 0;
    synthetic_output_count = 0;
    namedChunkTextBoxes: {[key: string]: SVGTextElement } = {};

    constructor(options: AtomicFlowBlockOptions, blockId: string, synthetic_input_count?: number, synthetic_output_count?: number) {
        this.id = blockId;
        if (!(options.message)) {
            throw new Error("'message' property is required to create a block");
        }

        if (synthetic_input_count) {
            this.synthetic_input_count = synthetic_input_count;
        }

        if (synthetic_output_count) {
            this.synthetic_output_count = synthetic_output_count;
        }

        [this.options, this.synthetic_input_count, this.synthetic_output_count ] = AtomicFlowBlock.add_synth_io(options,
                                                                                                                synthetic_input_count,
                                                                                                                synthetic_output_count);
        this.options.on_io_selected = options.on_io_selected;
        this.options.on_dropdown_extended = options.on_dropdown_extended;
        this.options.on_inputs_changed = options.on_inputs_changed;

        this.input_groups = [];
        this.output_groups = [];
        this.input_count = [];


        this.chunks = parse_chunks(this.options.message);
        for (const chunk of this.chunks) {
            if (chunk.type === 'named_var') {
                if (options.slots && options.slots[chunk.name]) {
                    chunk.val = options.slots[chunk.name];
                }
            }
        }

        this.chunkBoxes = [];
    }

    public dispose() {
        this.canvas.removeChild(this.group);
    }

    public static add_synth_io(options: AtomicFlowBlockOptions,
                               synthetic_input_count?: number,
                               synthetic_output_count?: number): [AtomicFlowBlockOptions, number, number] {
        synthetic_input_count = synthetic_input_count || 0;
        synthetic_output_count = synthetic_output_count || 0;

        options = JSON.parse(JSON.stringify(options));

        // Update inputs
        if (!options.inputs) {
            options.inputs = [];
        }

        // Update outputs
        if (!options.outputs) {
            options.outputs = [];
        }

        if (!synthetic_input_count && AtomicFlowBlock.required_synth_inputs(options) > 0) {
            options.inputs = ([ { type: "pulse" } ] as InputPortDefinition[]).concat(options.inputs);
            synthetic_input_count++;
        }

        if (!synthetic_output_count && AtomicFlowBlock.required_synth_outputs(options) > 0) {
            options.outputs = ([ { type: "pulse" } ] as OutputPortDefinition[]).concat(options.outputs);
            synthetic_output_count++;
        }

        return [options, synthetic_input_count, synthetic_output_count];
    }

    public static required_synth_outputs(options: AtomicFlowBlockOptions): number {
        let num_outputs = 0;

        if (options.type !== 'getter') {
            let has_pulse_output = false;
            for (const output of options.outputs || []) {
                if (is_pulse(output)) {
                    has_pulse_output = true;
                    break;
                }
            }

            if (!has_pulse_output) {
                num_outputs = 1;
            }
        }

        return num_outputs;
    }

    public static required_synth_inputs(options: AtomicFlowBlockOptions): number {
        let num_inputs = 0;

        if (['trigger', 'getter'].indexOf(options.type) < 0) {
            let has_pulse_input = false;
            for (const input of options.inputs || []) {
                if (!input) {
                    throw new Error(`Empty input on ${options.inputs}`);
                }

                if (is_pulse(input)) {
                    has_pulse_input = true;
                    break;
                }
            }

            if (!has_pulse_input) {
                num_inputs++;
            }
        }

        return num_inputs;
    }

    // Render elements
    private group: SVGGElement;
    private node: SVGGElement;
    private rect: SVGRectElement;
    private rectShadow: SVGRectElement;
    private canvas: SVGElement;
    private icon: SVGImageElement;
    private iconPlate: SVGRectElement;
    private iconSeparator: SVGPathElement;

    private chunkGroup: SVGGElement;
    private chunkBoxes: SVGElement[];
    private chunks: MessageChunk[];

    private position: {x: number, y: number};
    private input_count: number[];

    private input_x_position: number;
    private output_x_position: number;

    // I/O groups
    private input_groups: SVGElement[];
    private output_groups: SVGElement[];

    public static GetBlockType(): string {
        return BLOCK_TYPE;
    }

    public serialize(): AtomicFlowBlockData {
        return {
            type: BLOCK_TYPE,
            value: {
                options: JSON.parse(JSON.stringify(this.options)),
                slots: this.getSlots(),
                synthetic_input_count: this.synthetic_input_count,
                synthetic_output_count: this.synthetic_output_count,
            },
        }
    }

    public static Deserialize(data: AtomicFlowBlockData, blockId: string, manager: BlockManager): FlowBlock {
        if (data.type !== BLOCK_TYPE){
            throw new Error(`Block type mismatch, expected ${BLOCK_TYPE} found: ${data.type}`);
        }

        const options: AtomicFlowBlockOptions = JSON.parse(JSON.stringify(data.value.options));
        options.on_dropdown_extended = manager.onDropdownExtended.bind(manager);
        options.on_inputs_changed = manager.onInputsChanged.bind(manager);
        options.on_io_selected = manager.onIoSelected.bind(manager);

        const block = new AtomicFlowBlock(options,
                                          blockId,
                                          data.value.synthetic_input_count,
                                          data.value.synthetic_output_count,
                                         );

        for (const slot of Object.keys(data.value.slots || {})) {
            const chunk = block.chunks.find((val) => val.type === 'named_var'  && val.name === slot );
            block.updateChunk(chunk, data.value.slots[slot]);
        }

        return block;
    }

    public getBodyElement(): SVGGraphicsElement {
        if (!this.group) {
            throw Error("Not rendered");
        }

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

    public getOffset(): {x: number, y: number} {
        return {x: this.position.x, y: this.position.y};
    }

    public moveTo(pos: Position2D) {
        this.position.x = pos.x;
        this.position.y = pos.y;

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)
    }

    public updateOptions(blockData: FlowBlockData): void {
        const data = blockData as AtomicFlowBlockData;
        for (const var_name of Object.keys(data.value.slots)) {
            const value = data.value.slots[var_name];
            const chunk = this.chunks.find(p => p.type === 'named_var' && p.name == var_name);
            if (!chunk) {
                console.error(`Chunk not found for updating. Expected name: ${var_name}. New value: ${value}.`);
                continue;
            }

            const prevValue = (chunk as NamedVarMessageChunk).val;

            if (this._workspace) {
                this._workspace._notifyChangedVariable(prevValue, value);
            }

            this.updateChunk(chunk, value);
        }
    }

    private onOptionsUpdate() {
        if (this._workspace) {
            this._workspace.onBlockOptionsChanged(this);
        }
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

    public getPositionOfInput(index: number, edge?: boolean): Position2D {
        const group = this.input_groups[index];
        const circle = group.getElementsByTagName('circle')[0];

        const position = { x: parseInt(circle.getAttributeNS(null, 'cx')),
                           y: parseInt(circle.getAttributeNS(null, 'cy')),
                         };

        if (edge) {
            position.y -= INPUT_PORT_REAL_SIZE;
        }

        return position;
    }

    public getPositionOfOutput(index: number, edge?: boolean): Position2D {
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

    public addConnection(direction: 'in' | 'out', input_index: number, _block: FlowBlock, sourceType: string): boolean {
        if (direction === 'out') { return false; }

        if (!this.input_count[input_index]) {
            this.input_count[input_index] = 0;
        }
        this.input_count[input_index]++;

        const extra_opts = this.options.extra_inputs;
        if (extra_opts) {
            // Consider need for extra inputs
            let has_available_inputs = false;
            for (let i = 0; i < this.options.inputs.length; i++) {
                if (!this.input_count[i]) {
                    has_available_inputs = true;
                    break;
                }
            }

            if (!has_available_inputs) {
                // No available inputs, *might* need to create some more

                if ((extra_opts.quantity === 'any')
                    || (extra_opts.quantity.max < this.input_groups.length)) {

                    // Create new input
                    let input_index = this.input_groups.length;

                    const input = { type: extra_opts.type };

                    this.addInput(input, input_index);
                    this.options.inputs.push(input);
                    this.updateBody();
                    if (this.options.on_inputs_changed) {
                        this.options.on_inputs_changed(this, input_index);
                    }
                }
            }
        }

        // Consider updating the output pulse type
        const changedOutput = this.refreshInputConnection(input_index, sourceType);

        return changedOutput;
    }

    public removeConnection(direction: 'in' | 'out', index: number): boolean {
        if (direction === 'out') { return; }

        if (this.input_count[index]) {
            this.input_count[index]--;
        }


        // Consider updating the output pulse type
        const origType = this.options.inputs[index].type as ('pulse' | 'user-pulse');
        const changedOutput = this.refreshInputConnection(index, origType);

        return changedOutput;
    }

    public refreshConnectionTypes(linksFrom: [FlowConnectionData, SVGElement][],
                                  linksTo:   [FlowConnectionData, SVGElement][],
                                 ) {
        for (const [link, _element] of linksTo) {
            const index = link.sink.input_index;

            const linkType = link.type;
            this.refreshInputConnection(index, linkType);
        }

        for (const [link, element] of linksFrom) {
            const idx = link.source.output_index;

            if (this.overridenOutputTypes[idx]) {
                setConnectionType(this.overridenOutputTypes[idx], link, element);
            }
        }
    }

    private refreshInputConnection(index: number, linkType: string) : boolean {
        let changedOutput = false;

        if ((!this.options.fixed_pulses)
            && is_pulse(this.options.inputs[index])
            && ((linkType === 'pulse') || (linkType === 'user-pulse'))
           ) {
            this.setInPulseType(index, linkType);

            for (let outIndex = 0;
                 (this.options.outputs
                     && outIndex < this.options.outputs.length);
                 outIndex++) {

                if (is_pulse(this.options.outputs[outIndex])) {
                    this.setOutPulseType(outIndex, linkType);

                    changedOutput = true;
                }
            }

        }
        return changedOutput;
    }

    private setInPulseType(inputIndex: number, sourceType: 'pulse' | 'user-pulse') {
        const inClass = this.input_groups[inputIndex].getElementsByClassName('external_port')[0].classList;
        inClass.remove('pulse_port');
        inClass.remove('user-pulse_port');

        inClass.add(sourceType + '_port');

        this.overridenInputTypes[inputIndex] = sourceType;
    }

    private setOutPulseType(outputIndex: number, sourceType: 'pulse' | 'user-pulse') {
        const outClass = this.output_groups[outputIndex].getElementsByClassName('external_port')[0].classList;
        outClass.remove('pulse_port');
        outClass.remove('user-pulse_port');

        outClass.add(sourceType + '_port');

        this.overridenOutputTypes[outputIndex] = sourceType;
    }

    private addInput(input: InputPortDefinition, index: number) {
        const inputs_x_margin = 10; // px
        const input_plating_x_margin = 3; // px

        const in_group = document.createElementNS(SvgNS, 'g');
        in_group.classList.add('input');
        this.group.appendChild(in_group);

        const port_external = document.createElementNS(SvgNS, 'circle');
        const port_internal = document.createElementNS(SvgNS, 'circle');

        in_group.appendChild(port_external);
        in_group.appendChild(port_internal);

        const input_port_size = 50;
        const input_port_internal_size = 5;
        const input_position_start = this.input_x_position;
        let input_position_end = this.input_x_position + input_port_size;

        if (input.name) {
            // Bind input name and port
            const port_plating = document.createElementNS(SvgNS, 'rect');
            in_group.appendChild(port_plating);

            const text = document.createElementNS(SvgNS, 'text');
            text.textContent = input.name;
            text.setAttributeNS(null, 'class', 'argument_name input');
            in_group.appendChild(text);

            input_position_end = Math.max(input_position_end, (this.input_x_position
                                                               + text.getBoundingClientRect().width
                                                               + input_plating_x_margin * 2));
            const input_width = input_position_end - input_position_start;

            text.setAttributeNS(null, 'x', input_position_start + input_width/2 - text.getBoundingClientRect().width/2  + '');
            text.setAttributeNS(null, 'y', (INPUT_PORT_REAL_SIZE + text.getBoundingClientRect().height/3) + '' );

            this.input_x_position = input_position_end + inputs_x_margin;

            const input_height = Math.max(input_port_size / 2, (INPUT_PORT_REAL_SIZE
                                                                + text.getBoundingClientRect().height));

            // Configure port connector now that we know where the input will be positioned
            port_plating.setAttributeNS(null, 'class', 'port_plating');
            port_plating.setAttributeNS(null, 'x', input_position_start + '');
            port_plating.setAttributeNS(null, 'y', '0'); // Node stroke-width /2
            port_plating.setAttributeNS(null, 'width', (input_position_end - input_position_start) + '');
            port_plating.setAttributeNS(null, 'height', input_height/1.5 + '');
        }
        else {
            this.input_x_position += input_port_size;
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
            const element_index = index; // Capture for use in callback
            in_group.onclick = ((_ev: MouseEvent) => {
                this.options.on_io_selected(this, 'in', element_index, input,
                                            { x: port_x_center, y: port_y_center });
            });
        }

        this.input_groups[index] = in_group;
    }

    private updateChunk(chunk: MessageChunk, new_value: string) {
        if (chunk.type === 'const') {
            console.warn('Constant value chunks cannot be updated');
            return;
        }
        if (chunk.type === 'index_var') {
            console.warn('Indexed value chunks cannot be updated');
            return;
        }

        chunk.val = new_value;
        if (this.namedChunkTextBoxes[chunk.name]) {
            // Might not exist before initialization
            this.namedChunkTextBoxes[chunk.name].textContent = new_value;
            this.updateBody();
        }
    }

    private updateBody() {
        const MIN_WIDTH = 100;
        const X_PADDING = 5; // px
        const PLATE_X_PADDING = 2; // px
        const IMAGE_X_PADDING = 2; // px

        let x_offset = 0;
        if (this.icon) {
            const icon_rect = this.icon.getBBox();
            x_offset += icon_rect.width + icon_rect.x * 2;
        }


        let chunks_width = 0;
        for (let i = 0; i < this.chunks.length; i++) {
            if (this.chunks[i].type === 'const') {
                chunks_width += this.chunkBoxes[i].getBoundingClientRect().width + X_PADDING;
            }
            else if (this.chunks[i].type === 'named_var') {
                const group = this.chunkBoxes[i];
                const image = group.getElementsByClassName('var_dropdown_icon')[0];

                const text = group.getElementsByClassName('var_name')[0];
                chunks_width += text.getBoundingClientRect().width + image.getBoundingClientRect().width
                    + X_PADDING + PLATE_X_PADDING * 2 + IMAGE_X_PADDING * 2;
            }
            else if (this.chunks[i].type === 'index_var') {
                chunks_width += CONNECTOR_SIDE_SIZE + X_PADDING + PLATE_X_PADDING * 2;
            }
        }

        let widest_section = MIN_WIDTH;
        widest_section = Math.max(widest_section, chunks_width + X_PADDING * 2);

        // Both input and output already accout for the x_offset
        widest_section = Math.max(widest_section, this.input_x_position - x_offset);
        widest_section = Math.max(widest_section, this.output_x_position - x_offset);

        let next_chunk_position = x_offset + widest_section / 2 - chunks_width / 2;
        for (let i = 0; i < this.chunks.length; i++) {

            const chunk = this.chunks[i];
            if (chunk.type === 'const') {
                this.chunkBoxes[i].setAttributeNS(null, 'x', next_chunk_position + '');
                next_chunk_position += this.chunkBoxes[i].getBoundingClientRect().width + X_PADDING;
            }
            else if (chunk.type === 'named_var') {
                const group = this.chunkBoxes[i];
                const text = group.getElementsByClassName('var_name')[0];
                const plate = group.getElementsByClassName('var_plate')[0];
                const image = group.getElementsByClassName('var_dropdown_icon')[0];

                text.setAttributeNS(null, 'x', next_chunk_position + PLATE_X_PADDING * 2
                                    + '');

                const text_width = text.getBoundingClientRect().width;

                image.setAttributeNS(null, 'x', next_chunk_position + PLATE_X_PADDING * 2 + text_width + IMAGE_X_PADDING
                                     + '');

                const image_width = image.getBoundingClientRect().width;

                plate.setAttributeNS(null, 'x', next_chunk_position + '');
                plate.setAttributeNS(null, 'width', text_width + image_width + PLATE_X_PADDING * 2 + IMAGE_X_PADDING * 2 + "");

                next_chunk_position += text_width + image_width + X_PADDING + PLATE_X_PADDING * 2 + IMAGE_X_PADDING * 2;
            }
            else if (chunk.type === 'index_var') {
                const group = this.chunkBoxes[i];
                const connector = group.getElementsByClassName('var_connector')[0];
                const path = group.getElementsByClassName('var_path')[0];

                connector.setAttributeNS(null, 'x', next_chunk_position + '');
                connector.setAttributeNS(null, 'width', CONNECTOR_SIDE_SIZE + "");

                let target: Position2D;
                if (chunk.direction === 'in'){
                    target = this.getPositionOfInput(chunk.index + this.synthetic_input_count);
                    target.y += INPUT_PORT_REAL_SIZE / 2;
                }
                else if (chunk.direction === 'out'){
                    target = this.getPositionOfOutput(chunk.index + this.synthetic_output_count);
                    target.y -= OUTPUT_PORT_REAL_SIZE / 2;
                }

                const conn_area = (connector as any).getBBox() as Area2D;
                let off: Position2D;
                if (conn_area.y > target.y) { // Under input, start on connector's top
                    off = {
                        x: conn_area.x + conn_area.width / 2,
                        y: conn_area.y,
                    };
                }
                else { // Over output, start on connector's bottom
                    off = {
                        x: conn_area.x + conn_area.width / 2,
                        y: conn_area.y + conn_area.height,
                    };
                }

                path.setAttributeNS(null, 'd', `M${off.x},${off.y} ${target.x},${target.y}`);

                next_chunk_position += CONNECTOR_SIDE_SIZE + X_PADDING + PLATE_X_PADDING * 2;
            }
        }

        const box_width = widest_section + x_offset;
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");
    }

    public getBlockContextActions(): BlockContextAction[] {
        return [];
    }

    public getSlots(): {[key: string]: string} {
        const slots: {[key: string]: string} = {};
        for (const chunk of this.chunks) {
            if (chunk.type === 'named_var') {
                slots[chunk.name] = chunk.val;
            }
        }

        return slots;
    }

    public getInputs(): InputPortDefinition[] {
        if (!this.options.inputs) { return []; }
        return JSON.parse(JSON.stringify(this.options.inputs));
    }

    public getOutputType(index: number): string {
        if (this.overridenOutputTypes[index]) {
            return this.overridenOutputTypes[index];
        }

        return this.options.outputs[index].type;
    }

    public getInputType(index: number): string {
        if (this.overridenInputTypes[index]) {
            return this.overridenInputTypes[index];
        }

        return this.options.inputs[index].type;
    }

    public getOutputRunwayDirection(): Direction2D {
        return 'down';
    }

    private getCorrespondingInputIndex(chunk: MessageChunk): number {
        if (chunk.type !== 'index_var') {
            return null;
        }
        return (chunk.index + this.synthetic_input_count); // Skip inputs not specified by the user
    }

    private getCorrespondingOutputIndex(chunk: MessageChunk): number {
        if (chunk.type !== 'index_var') {
            return null;
        }
        return (chunk.index + this.synthetic_output_count); // Skip outputs not specified by the user
    }

    public render(canvas: SVGElement, initOpts: FlowBlockInitOpts): SVGElement {
        this.canvas = canvas;
        this._workspace = initOpts.workspace;

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

        if (this.group) { return this.group }

        const y_padding = 5; // px
        const input_initial_x_position = 10; // px

        const output_initial_x_position = 10; // px
        const outputs_x_margin = 10; // px
        const output_plating_x_margin = 3; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');
        this.chunkGroup = document.createElementNS(SvgNS, 'g');

        this.group.setAttribute('class', 'flow_node atomic_node');

        this.node.appendChild(this.rectShadow);
        this.node.appendChild(this.rect);
        this.node.appendChild(this.chunkGroup);
        this.group.appendChild(this.node);
        this.canvas.appendChild(this.group);

        if (this.options.icon) {
            this.icon = document.createElementNS(SvgNS, 'image');
            this.icon.setAttributeNS(null, 'class', 'node_icon');
            this.icon.setAttributeNS(null, 'href', this.options.icon);
            this.icon.setAttributeNS(null, 'width', '4ex');
            this.icon.setAttributeNS(null, 'height', '4ex');
            this.icon.setAttributeNS(null, 'x', ICON_PADDING);

            this.iconPlate = document.createElementNS(SvgNS, 'rect');
            this.iconPlate.setAttributeNS(null, 'class', 'node_icon_plate');
            this.iconPlate.setAttributeNS(null, 'x', '1.5');
            this.iconPlate.setAttributeNS(null, 'y', '1.5');

            this.iconSeparator = document.createElementNS(SvgNS, 'path');
            this.iconSeparator.setAttributeNS(null, 'class', 'node_icon_separator');

            this.group.appendChild(this.iconPlate);
            this.group.appendChild(this.iconSeparator);
            this.group.appendChild(this.icon);
        }

        // Calculate text correction
        const refText = document.createElementNS(SvgNS, 'text');
        refText.setAttribute('class', 'node_name');
        refText.setAttributeNS(null,'textlength', '100%');

        refText.setAttributeNS(null, 'x', "0");
        refText.setAttributeNS(null, 'y', "0");
        refText.textContent = "test";
        this.canvas.appendChild(refText);

        const refBox = refText.getBoundingClientRect();
        this.canvas.removeChild(refText);
        // End of text correction calculation

        const box_height = (refBox.height * 3 + y_padding * 2);

        // Add inputs
        this.input_x_position = input_initial_x_position;
        this.output_x_position = output_initial_x_position;

        if (this.icon) {
            const icon_rect = this.icon.getBBox();
            this.input_x_position += icon_rect.width;
            this.output_x_position += icon_rect.width;
        }

        let input_index = -1;

        for (const input of this.options.inputs) {
            input_index++;

            this.addInput(input, input_index);
        }

        // Add outputs
        let output_index = -1;

        for (const output of this.options.outputs) {
            output_index++;

            const out_group = document.createElementNS(SvgNS, 'g');
            out_group.classList.add('output');
            this.group.appendChild(out_group);

            const port_external = document.createElementNS(SvgNS, 'circle');
            const port_internal = document.createElementNS(SvgNS, 'circle');

            out_group.appendChild(port_external);
            out_group.appendChild(port_internal);

            const output_port_size = 50;
            const output_port_internal_size = 5;
            const output_position_start = this.output_x_position;
            let output_position_end = this.output_x_position + output_port_size;


            if (output.name) {
                // Bind output name and port
                const port_plating = document.createElementNS(SvgNS, 'rect');
                out_group.appendChild(port_plating);

                const text = document.createElementNS(SvgNS, 'text');
                text.textContent = output.name;
                text.setAttributeNS(null, 'class', 'argument_name output');
                out_group.appendChild(text);

                output_position_end = Math.max(output_position_end, (this.output_x_position
                                                                     + text.getBoundingClientRect().width
                                                                     + output_plating_x_margin * 2));

                const output_width = output_position_end - output_position_start;

                text.setAttributeNS(null, 'x', output_position_start + output_width/2 - text.getBoundingClientRect().width/2  + '');
                text.setAttributeNS(null, 'y', box_height - (OUTPUT_PORT_REAL_SIZE/2) + '' );

                this.output_x_position = output_position_end + outputs_x_margin;

                const output_height = Math.max(output_port_size / 2, (OUTPUT_PORT_REAL_SIZE
                                                                      + text.getBoundingClientRect().height));

                // Configure port connector now that we know where the output will be positioned
                port_plating.setAttributeNS(null, 'class', 'port_plating');
                port_plating.setAttributeNS(null, 'x', output_position_start + '');
                port_plating.setAttributeNS(null, 'y', box_height - output_height/1.5 + '');
                port_plating.setAttributeNS(null, 'width', (output_position_end - output_position_start) + '');
                port_plating.setAttributeNS(null, 'height', output_height/1.5 + '');

            }
            else {
                this.output_x_position += output_port_size;
            }

            let type_class = 'unknown_type';
            if (output.type) {
                type_class = output.type + '_port';
            }

            // Draw the output port
            const port_x_center = (output_position_start + output_position_end) / 2;
            const port_y_center = box_height;

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

        // Draw chunks
        for (const chunk of this.chunks) {
            if (chunk.type === 'const') {
                const text = document.createElementNS(SvgNS, 'text');
                this.chunkGroup.appendChild(text);

                text.setAttribute('class', 'node_name');
                text.setAttributeNS(null,'textlength', '100%');
                text.setAttributeNS(null, 'y', box_height/1.75 + "");
                text.textContent = chunk.val;

                this.chunkBoxes.push(text);
            }
            else if (chunk.type === 'named_var') {
                const group = document.createElementNS(SvgNS, 'g');
                group.setAttributeNS(null, 'class', 'named_var');
                this.chunkGroup.appendChild(group);

                const plate = document.createElementNS(SvgNS, 'rect');
                const text = document.createElementNS(SvgNS, 'text');
                const image = document.createElementNS(SvgNS, 'image');

                group.appendChild(plate);
                group.appendChild(text);
                group.appendChild(image);

                text.setAttribute('class', 'var_name dropdown_value');
                text.setAttributeNS(null,'textlength', '100%');
                text.setAttributeNS(null, 'y', box_height/1.75 + "");
                text.textContent = chunk.val;

                plate.setAttribute('class', 'var_plate');
                plate.setAttributeNS(null, 'y', box_height/2 - refBox.height  + "");
                plate.setAttributeNS(null, 'height', refBox.height * 2 + "");

                image.setAttributeNS(null, 'class', 'var_dropdown_icon');
                image.setAttributeNS(null, 'href', '/assets/sprites/expand_more.svg');
                image.setAttributeNS(null, 'width', '2ex');
                image.setAttributeNS(null, 'height', '2ex');
                image.setAttributeNS(null, 'y', box_height/2 - image.getBoundingClientRect().height/2  + "");

                this.namedChunkTextBoxes[chunk.name] = text;

                if (this.options.on_dropdown_extended) {
                    group.onclick = () => {
                        this.options.on_dropdown_extended(this,
                                                          chunk.name,
                                                          chunk.val,
                                                          plate.getBBox(),
                                                          (new_value: string) => {
                                                              this.updateChunk(chunk, new_value);
                                                              this.onOptionsUpdate();
                                                          }
                                                         );
                    };
                }

                this.chunkBoxes.push(group);
            }
            else if (chunk.type === 'index_var') {
                const group = document.createElementNS(SvgNS, 'g');
                group.setAttributeNS(null, 'class', 'index_var');
                this.chunkGroup.appendChild(group);

                const connector = document.createElementNS(SvgNS, 'rect');
                const path = document.createElementNS(SvgNS, 'path');

                group.appendChild(connector);
                group.appendChild(path);

                let type = 'any';
                if (chunk.direction === 'in'){
                    const inp = this.options.inputs[this.getCorrespondingInputIndex(chunk)];
                    if (!inp) {
                        console.error(chunk, this.options.inputs, this.chunks);
                    }
                    else {
                        type = inp.type || type;
                    }
                }
                else if (chunk.direction === 'out'){
                    const outp = this.options.outputs[this.getCorrespondingOutputIndex(chunk)];
                    if (!outp) {
                        console.error(chunk, this.options.outputs, this.chunks);
                    }
                    else {
                        type = outp.type || type;
                    }
                }

                connector.setAttribute('class', `var_connector direction_${chunk.direction} ${type}_port`);
                connector.setAttributeNS(null, 'y', box_height/2 - CONNECTOR_SIDE_SIZE / 2  + "");
                connector.setAttributeNS(null, 'height', CONNECTOR_SIDE_SIZE + "");

                path.setAttribute('class', `var_path direction_${chunk.direction} ${type}_port`);

                this.chunkBoxes.push(group);
            }
        }

        // Properly place elements
        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'rx', "2px"); // Like border-radius, in px

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'rx', "2px"); // Like border-radius, in px

        if (this.icon) {
            const icon_rect = this.icon.getBBox();
            this.icon.setAttributeNS(null, 'y', box_height / 2 - icon_rect.height / 2 + '');
            const padding_px = icon_rect.x;

            const separator_x = icon_rect.width + padding_px * 2;
            this.iconSeparator.setAttributeNS(null, 'd', `M${ separator_x },0 L${separator_x},${box_height}`);

            this.iconPlate.setAttributeNS(null, 'width', separator_x + 1 + ''); // +1 to cover separator stroke-width
            this.iconPlate.setAttributeNS(null, 'height', box_height - 3 + '');

        }

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        this.updateBody();

        return this.group;
    }

}
