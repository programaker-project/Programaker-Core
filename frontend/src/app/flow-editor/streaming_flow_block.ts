import {
    FlowBlock, FlowBlockOptions,
    InputPortDefinition,
    Area2D, Direction2D, Position2D
} from './flow_block';

const SvgNS = "http://www.w3.org/2000/svg";

const INPUT_PORT_REAL_SIZE = 10;
const OUTPUT_PORT_REAL_SIZE = 10;

export class StreamingFlowBlock implements FlowBlock {
    options: FlowBlockOptions;

    constructor(options: FlowBlockOptions) {
        if (!(options.message)) {
            throw new Error("'message' property is required to create a block");
        }

        this.options = options;
        this.input_groups = [];
        this.output_groups = [];
        this.inputs_taken = [];
    }

    public dispose() {
        this.canvas.removeChild(this.group);
    }

    // Render elements
    private group: SVGElement;
    private node: SVGElement;
    private rect: SVGElement;
    private textBox: SVGElement;
    private canvas: SVGElement;

    private position: {x: number, y: number};
    private textCorrection: {x: number, y: number};
    private inputs_taken: boolean[];

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

    public addConnection(input_index: number) {
        this.inputs_taken[input_index] = true;

        const extra_opts = this.options.extra_inputs;
        if (!extra_opts) {
            return;
        }

        // Consider need for extra inputs
        let has_available_inputs = false;
        for (let i = 0; i < this.options.inputs.length; i++) {
            if (!this.inputs_taken[i]) {
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
            text.setAttributeNS(null, 'y', INPUT_PORT_REAL_SIZE + this.textCorrection.y + text.getClientRects()[0].height/3
                                           + '' );

            this.input_x_position = input_position_end + inputs_x_margin;

            const input_height = Math.max(input_port_size / 2, (INPUT_PORT_REAL_SIZE
                                                                + text.getClientRects()[0].height));

            // Configure port connector now that we know where the input will be positioned
            port_plating.setAttributeNS(null, 'class', 'port_plating');
            port_plating.setAttributeNS(null, 'x', input_position_start + '');
            port_plating.setAttributeNS(null, 'y', '0');
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

        this.input_groups.push(in_group);
        if (this.options.on_io_selected) {
            in_group.onclick = ((_ev: MouseEvent) => {
                this.options.on_io_selected(this, 'in', index, input,
                                            { x: port_x_center, y: port_y_center });
            });
        }
    }

    private updateBody() {
        const min_width = 100;
        const x_padding = 5; // px


        let widest_section = min_width;
        widest_section = Math.max(widest_section, this.textBox.getClientRects()[0].width + x_padding * 2);
        widest_section = Math.max(widest_section, this.input_x_position);
        widest_section = Math.max(widest_section, this.output_x_position);

        const box_width = widest_section;

        // Center text box
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.textBox.setAttributeNS(null, 'x', (this.textCorrection.x
                                                + (box_width/2)
                                                - (this.textBox.getClientRects()[0].width/2)) + "");
    }

    public getInputs(): InputPortDefinition[] {
        if (!this.options.inputs) { return []; }

        return JSON.parse(JSON.stringify(this.options.inputs));
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

    public getOutputRunwayDirection(): Direction2D {
        return 'down';
    }

    public getOutputType(index: number): string {
        return this.options.outputs[index].type;
    }

    public render(canvas: SVGElement, position?: {x: number, y: number}): SVGElement {
        this.canvas = canvas;
        if (position) {
            this.position = { x: position.x, y: position.y };
        }
        else {
            this.position = {x: 0, y: 0};
        }

        if (this.group) { return this.group }

        const y_padding = 5; // px
        const input_initial_x_position = 10; // px

        const output_initial_x_position = 10; // px
        const outputs_x_margin = 10; // px
        const output_plating_x_margin = 3; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.textBox = document.createElementNS(SvgNS, 'text');

        this.group.setAttribute('class', 'flow_node streaming_node');
        this.textBox.setAttribute('class', 'node_name');
        this.textBox.textContent = this.options.message;
        this.textBox.setAttributeNS(null,'textlength', '100%');

        this.textBox.setAttributeNS(null, 'x', "0");
        this.textBox.setAttributeNS(null, 'y', "0");

        this.node.appendChild(this.rect);
        this.node.appendChild(this.textBox);
        this.group.appendChild(this.node);
        this.canvas.appendChild(this.group);

        // Read text correction
        this.textCorrection = {
            x: -(this.textBox.getClientRects()[0].left - this.node.getClientRects()[0].left),
            y: -(this.textBox.getClientRects()[0].top - this.node.getClientRects()[0].top)
        };

        const box_height = (this.textBox.getClientRects()[0].height * 3 + y_padding * 2);

        // Add inputs
        this.input_x_position = input_initial_x_position + this.textCorrection.x;
        let input_index = -1;

        for (const input of this.options.inputs || []) {
            input_index++;

            this.addInput(input, input_index);
        }

        // Add outputs
        this.output_x_position = output_initial_x_position + this.textCorrection.x;
        let output_index = -1;
        for (const output of this.options.outputs || []) {
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
                                                - (OUTPUT_PORT_REAL_SIZE / 2)) + '' );

                this.output_x_position = output_position_end + outputs_x_margin;

                const output_height = Math.max(output_port_size / 2, (OUTPUT_PORT_REAL_SIZE
                                                                      + text.getClientRects()[0].height));

                // Configure port connector now that we know where the output will be positioned
                port_plating.setAttributeNS(null, 'class', 'port_plating');
                port_plating.setAttributeNS(null, 'x', output_position_start + '');
                port_plating.setAttributeNS(null, 'y', box_height - output_height/1.5 + '');
                port_plating.setAttributeNS(null, 'width', (output_position_end - output_position_start) + '');
                port_plating.setAttributeNS(null, 'height', output_height /1.5 + '');

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
            this.output_groups.push(out_group);
        }

        this.textBox.setAttributeNS(null, 'y', this.textCorrection.y + box_height / 2 + "");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'height', box_height + "");

        this.rect.setAttributeNS(null, 'rx', "2px"); // Like border-radius, in px

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        this.updateBody();

        return this.group;
    }

}
