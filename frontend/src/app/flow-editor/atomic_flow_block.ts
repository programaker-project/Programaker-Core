import { FlowBlock, FlowBlockOptions,
         Area2D, Direction2D, Position2D,
         InputPortDefinition, OutputPortDefinition,
       } from './flow_block';

const SvgNS = "http://www.w3.org/2000/svg";

const INPUT_PORT_REAL_SIZE = 10;
const OUTPUT_PORT_REAL_SIZE = 10;
const CONNECTOR_SIDE_SIZE = 15;
const ICON_PADDING = '1ex';

export interface AtomicFlowBlockOptions extends FlowBlockOptions {
    type: 'operation' | 'getter' | 'trigger';
    icon?: string,
}

type MessageChunk = ( { type: 'const', val: string }
    | { type: 'named_var', val: string, name: string }
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
    options: AtomicFlowBlockOptions;
    synthetic_input_count = 0;
    synthetic_output_count = 0;

    constructor(options: AtomicFlowBlockOptions) {
        if (!(options.message)) {
            throw new Error("'message' property is required to create a block");
        }

        this.options = JSON.parse(JSON.stringify(options));
        this.options.on_io_selected = options.on_io_selected;
        this.options.on_dropdown_extended = options.on_dropdown_extended;
        this.options.on_inputs_changed = options.on_inputs_changed;

        this.input_groups = [];
        this.output_groups = [];
        this.input_count = [];

        // Update inputs
        if (!this.options.inputs) {
            this.options.inputs = [];
        }

        if (this.options.type !== 'trigger') {
            this.options.inputs = ([ { type: "pulse" } ] as InputPortDefinition[]).concat(this.options.inputs);
            this.synthetic_input_count++;
        }

        // Update outputs
        if (!this.options.outputs) {
            this.options.outputs = [];
        }

        this.chunks = parse_chunks(this.options.message);
        for (const chunk of this.chunks) {
            if (chunk.type === 'named_var') {
                if (options.slots && options.slots[chunk.name]) {
                    chunk.val = options.slots[chunk.name];
                }
            }
        }

        this.chunkBoxes = [];

        let has_pulse_output = false;
        for (const output of this.options.outputs) {
            if (output.type === 'pulse') {
                has_pulse_output = true;
                break;
            }
        }

        if (!has_pulse_output) {
            this.options.outputs = ([ { type: "pulse" } ] as OutputPortDefinition[]).concat(this.options.outputs);
            this.synthetic_output_count++;
        }
    }

    public dispose() {
        this.canvas.removeChild(this.group);
    }

    // Render elements
    private group: SVGElement;
    private node: SVGElement;
    private rect: SVGElement;
    private canvas: SVGElement;
    private icon: SVGImageElement;
    private iconPlate: SVGRectElement;
    private iconSeparator: SVGPathElement;

    private chunkGroup: SVGGElement;
    private chunkBoxes: SVGElement[];
    private chunks: MessageChunk[];

    private position: {x: number, y: number};
    private textCorrection: {x: number, y: number};
    private input_count: number[];

    private input_x_position: number;
    private output_x_position: number;

    // I/O groups
    private input_groups: SVGElement[];
    private output_groups: SVGElement[];

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

    public addConnection(direction: 'in' | 'out', input_index: number) {
        if (direction === 'out') { return; }

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

    public removeConnection(direction: 'in' | 'out', index: number) {
        if (direction === 'out') { return; }

        if (this.input_count[index]) {
            this.input_count[index]--;
        }
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
                                                               + text.getClientRects()[0].width
                                                               + input_plating_x_margin * 2));
            const input_width = input_position_end - input_position_start;

            text.setAttributeNS(null, 'x', input_position_start + input_width/2 - text.getClientRects()[0].width/2  + '');
            text.setAttributeNS(null, 'y', (INPUT_PORT_REAL_SIZE + this.textCorrection.y + text.getClientRects()[0].height/3) + '' );

            this.input_x_position = input_position_end + inputs_x_margin;

            const input_height = Math.max(input_port_size / 2, (INPUT_PORT_REAL_SIZE
                                                                + text.getClientRects()[0].height));

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

    private updateChunk(chunk: MessageChunk, textBox: SVGTextElement, new_value: string) {
        if (chunk.type === 'const') {
            console.warn('Constant value chunks cannot be updated');
            return;
        }
        if (chunk.type === 'index_var') {
            console.warn('Indexed value chunks cannot be updated');
            return;
        }

        chunk.val = new_value;
        textBox.textContent = new_value;
        this.updateBody();
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
                chunks_width += this.chunkBoxes[i].getClientRects()[0].width + X_PADDING;
            }
            else if (this.chunks[i].type === 'named_var') {
                const group = this.chunkBoxes[i];
                const image = group.getElementsByClassName('var_dropdown_icon')[0];

                const text = group.getElementsByClassName('var_name')[0];
                chunks_width += text.getClientRects()[0].width + image.getClientRects()[0].width
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
                this.chunkBoxes[i].setAttributeNS(null, 'x', this.textCorrection.x + next_chunk_position + '');
                next_chunk_position += this.chunkBoxes[i].getClientRects()[0].width + X_PADDING;
            }
            else if (chunk.type === 'named_var') {
                const group = this.chunkBoxes[i];
                const text = group.getElementsByClassName('var_name')[0];
                const plate = group.getElementsByClassName('var_plate')[0];
                const image = group.getElementsByClassName('var_dropdown_icon')[0];

                text.setAttributeNS(null, 'x', this.textCorrection.x
                                    + next_chunk_position + PLATE_X_PADDING * 2
                                    + '');

                const text_width = text.getClientRects()[0].width;

                image.setAttributeNS(null, 'x', next_chunk_position + PLATE_X_PADDING * 2 + text_width + IMAGE_X_PADDING
                                     + '');

                const image_width = image.getClientRects()[0].width;

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
    }

    public getSlots(): {[key: string]: string} {
        const slots = {};
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
        return this.options.outputs[index].type;
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

    public render(canvas: SVGElement, position?: {x: number, y: number}): SVGElement {
        this.canvas = canvas;
        if (position) {
            this.position = { x: position.x, y: position.y };
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

        const min_height = 25;

        const y_padding = 5; // px
        const input_initial_x_position = 10; // px

        const output_initial_x_position = 10; // px
        const outputs_x_margin = 10; // px
        const output_plating_x_margin = 3; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.chunkGroup = document.createElementNS(SvgNS, 'g');

        this.group.setAttribute('class', 'flow_node atomic_node');

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
            this.iconPlate.setAttributeNS(null, 'x', '0');
            this.iconPlate.setAttributeNS(null, 'y', '0');
            this.iconPlate.setAttributeNS(null, 'rx', '2');

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

        const refBox = refText.getClientRects()[0];
        this.textCorrection = { x: 0, y: 0 }; // TODO: Remove text correction
        this.canvas.removeChild(refText);
        // End of text correction calculation

        const box_height = (refBox.height * 3 + y_padding * 2);

        // Add inputs
        this.input_x_position = input_initial_x_position + this.textCorrection.x;
        this.output_x_position = output_initial_x_position + this.textCorrection.x;

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
                                                                     + text.getClientRects()[0].width
                                                                     + output_plating_x_margin * 2));

                const output_width = output_position_end - output_position_start;

                text.setAttributeNS(null, 'x', output_position_start + output_width/2 - text.getClientRects()[0].width/2  + '');
                text.setAttributeNS(null, 'y', (this.textCorrection.y + box_height
                                                - (OUTPUT_PORT_REAL_SIZE/2)) + '' );

                this.output_x_position = output_position_end + outputs_x_margin;

                const output_height = Math.max(output_port_size / 2, (OUTPUT_PORT_REAL_SIZE
                                                                      + text.getClientRects()[0].height));

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
                text.setAttributeNS(null, 'y', this.textCorrection.y + box_height/1.75 + "");
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
                text.setAttributeNS(null, 'y', this.textCorrection.y + box_height/1.75 + "");
                text.textContent = chunk.val;

                plate.setAttribute('class', 'var_plate');
                plate.setAttributeNS(null, 'y', box_height/2 - refBox.height  + "");
                plate.setAttributeNS(null, 'height', refBox.height * 2 + "");

                image.setAttributeNS(null, 'class', 'var_dropdown_icon');
                image.setAttributeNS(null, 'href', '/assets/sprites/expand_more.svg');
                image.setAttributeNS(null, 'width', '2ex');
                image.setAttributeNS(null, 'height', '2ex');
                image.setAttributeNS(null, 'y', box_height/2 - image.getClientRects()[0].height/2  + "");

                if (this.options.on_dropdown_extended) {
                    group.onclick = () => {
                        this.options.on_dropdown_extended(this,
                                                          chunk.name,
                                                          chunk.val,
                                                          plate.getBBox(),
                                                          (new_value: string) => {
                                                              this.updateChunk(chunk, text, new_value);
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
                        type = inp.type || type; // Messages are 1-indexed
                    }
                }
                else if (chunk.direction === 'out'){
                    const outp = this.options.outputs[this.getCorrespondingOutputIndex(chunk)];
                    if (!outp) {
                        console.error(chunk, this.options.outputs, this.chunks);
                    }
                    else {
                        type = outp.type || type; // Messages are 1-indexed
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

        if (this.icon) {
            const icon_rect = this.icon.getBBox();
            this.icon.setAttributeNS(null, 'y', box_height / 2 - icon_rect.height / 2 + '');
            const padding_px = icon_rect.x;

            const separator_x = icon_rect.width + padding_px * 2;
            this.iconSeparator.setAttributeNS(null, 'd', `M${ separator_x },0 L${separator_x},${box_height}`);

            this.iconPlate.setAttributeNS(null, 'width', separator_x + 1 + ''); // +1 to cover separator stroke-width
            this.iconPlate.setAttributeNS(null, 'height', box_height + '');

        }

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        this.updateBody();

        return this.group;
    }

}
