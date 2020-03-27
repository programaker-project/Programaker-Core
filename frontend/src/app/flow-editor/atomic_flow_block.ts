import { FlowBlock, FlowBlockOptions,
         Position2D,
         InputPortDefinition, OutputPortDefinition,
       } from './flow_block';

const SvgNS = "http://www.w3.org/2000/svg";

export interface AtomicFlowBlockOptions extends FlowBlockOptions {
    type: 'operation' | 'getter' | 'trigger';
}

export class AtomicFlowBlock implements FlowBlock {
    options: AtomicFlowBlockOptions;

    constructor(options: AtomicFlowBlockOptions) {
        if (!(options.message)) {
            throw new Error("'message' property is required to create a block");
        }

        this.options = JSON.parse(JSON.stringify(options));
        this.options.on_io_selected = options.on_io_selected;

        this.input_groups = {};
        this.output_groups = {};

        // Update inputs
        if (!this.options.inputs) {
            this.options.inputs = [];
        }

        if (this.options.type !== 'trigger') {
            this.options.inputs = ([ { type: "pulse" } ] as InputPortDefinition[]).concat(this.options.inputs);
        }

        // Update outputs
        if (!this.options.outputs) {
            this.options.outputs = [];
        }

        this.options.outputs = ([ { type: "pulse" } ] as OutputPortDefinition[]).concat(this.options.outputs);
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

    // I/O groups
    private input_groups: {[key: number]: SVGElement};
    private output_groups: {[key: number]: SVGElement};

    public getBodyElement(): SVGElement {
        if (!this.group) {
            throw Error("Not rendered");
        }

        return this.node;
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

    public getPositionOfInput(index: number): Position2D {
        const group = this.input_groups[index];
        const circle = group.getElementsByTagName('circle')[0];
        return { x: parseInt(circle.getAttributeNS(null, 'cx')),
                 y: parseInt(circle.getAttributeNS(null, 'cy')),
               };
    }

    public getPositionOfOutput(index: number): Position2D {
        const group = this.output_groups[index];
        const circle = group.getElementsByTagName('circle')[0];
        return { x: parseInt(circle.getAttributeNS(null, 'cx')),
                 y: parseInt(circle.getAttributeNS(null, 'cy')),
               };
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

        const min_width = 100;
        const min_height = 25;

        const x_padding = 5; // px
        const y_padding = 5; // px
        const input_initial_x_position = 10; // px
        const inputs_x_margin = 10; // px
        const input_plating_x_margin = 3; // px

        const output_initial_x_position = 10; // px
        const outputs_x_margin = 10; // px
        const output_plating_x_margin = 3; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.textBox = document.createElementNS(SvgNS, 'text');

        this.group.setAttribute('class', 'flow_node atomic_node');
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
            x: -(this.textBox.getClientRects()[0].left - canvas.getClientRects()[0].left),
            y: -(this.textBox.getClientRects()[0].top - canvas.getClientRects()[0].top)
        };

        const box_height = (this.textBox.getClientRects()[0].height * 4.5 + y_padding * 2);

        // Add inputs
        let input_x_position = input_initial_x_position + this.textCorrection.x;
        let input_index = -1;

        for (const input of this.options.inputs) {
            input_index++;

            const in_group = document.createElementNS(SvgNS, 'g');
            this.group.appendChild(in_group);

            const input_port_size = 50;
            const input_port_real_size = 10;
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

                text.setAttributeNS(null, 'x', input_x_position + input_plating_x_margin + '');
                text.setAttributeNS(null, 'y', (input_port_real_size + this.textCorrection.y) + '' );

                input_position_end = Math.max(input_position_end, (input_x_position
                                                                   + text.getClientRects()[0].width
                                                                   + input_plating_x_margin * 2));
                input_x_position = input_position_end + inputs_x_margin;

                const input_height = Math.max(input_port_size / 2, (input_port_real_size
                                                                    + text.getClientRects()[0].height));

                // Configure port connector now that we know where the input will be positioned
                port_plating.setAttributeNS(null, 'class', 'port_plating');
                port_plating.setAttributeNS(null, 'x', input_position_start + '');
                port_plating.setAttributeNS(null, 'y', '1'); // Node stroke-width /2
                port_plating.setAttributeNS(null, 'width', (input_position_end - input_position_start) + '');
                port_plating.setAttributeNS(null, 'height', input_height + '');
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

            const port_external = document.createElementNS(SvgNS, 'circle');
            port_external.setAttributeNS(null, 'class', 'input external_port ' + type_class);
            port_external.setAttributeNS(null, 'cx', port_x_center + '');
            port_external.setAttributeNS(null, 'cy', port_y_center + '');
            port_external.setAttributeNS(null, 'r', input_port_real_size + '');

            const port_internal = document.createElementNS(SvgNS, 'circle');
            port_internal.setAttributeNS(null, 'class', 'input internal_port');
            port_internal.setAttributeNS(null, 'cx', port_x_center + '');
            port_internal.setAttributeNS(null, 'cy', port_y_center + '');
            port_internal.setAttributeNS(null, 'r', input_port_internal_size + '');

            in_group.appendChild(port_external);
            in_group.appendChild(port_internal);

            if (this.options.on_io_selected) {
                const element_index = input_index; // Capture for use in callback
                in_group.onclick = ((_ev: MouseEvent) => {
                    this.options.on_io_selected(this, 'in', element_index, input,
                                                { x: port_x_center, y: port_y_center });
                });
            }

            this.input_groups[input_index] = in_group;
        }

        // Add outputs
        let output_x_position = output_initial_x_position + this.textCorrection.x;
        let output_index = -1;

        for (const output of this.options.outputs) {
            output_index++;

            const out_group = document.createElementNS(SvgNS, 'g');
            this.group.appendChild(out_group);

            const output_port_size = 50;
            const output_port_real_size = 10;
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

                text.setAttributeNS(null, 'x', output_x_position + output_plating_x_margin + '');
                text.setAttributeNS(null, 'y', (this.textCorrection.y + box_height
                                                - (output_port_real_size + text.getClientRects()[0].height)) + '' );

                output_position_end = Math.max(output_position_end, (output_x_position
                                                                     + text.getClientRects()[0].width
                                                                     + output_plating_x_margin * 2));
                output_x_position = output_position_end + outputs_x_margin;

                const output_height = Math.max(output_port_size / 2, (output_port_real_size
                                                                      + text.getClientRects()[0].height));

                // Configure port connector now that we know where the output will be positioned
                port_plating.setAttributeNS(null, 'class', 'port_plating');
                port_plating.setAttributeNS(null, 'x', output_position_start + '');
                port_plating.setAttributeNS(null, 'y', box_height - output_height - 1 + ''); // -1 for node stroke-width /2
                port_plating.setAttributeNS(null, 'width', (output_position_end - output_position_start) + '');
                port_plating.setAttributeNS(null, 'height', output_height + '');

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
            const port_y_center = box_height;

            const port_external = document.createElementNS(SvgNS, 'circle');
            port_external.setAttributeNS(null, 'class', 'output external_port ' + type_class);
            port_external.setAttributeNS(null, 'cx', port_x_center + '');
            port_external.setAttributeNS(null, 'cy', port_y_center + '');
            port_external.setAttributeNS(null, 'r', output_port_real_size + '');

            const port_internal = document.createElementNS(SvgNS, 'circle');
            port_internal.setAttributeNS(null, 'class', 'output internal_port');
            port_internal.setAttributeNS(null, 'cx', port_x_center + '');
            port_internal.setAttributeNS(null, 'cy', port_y_center + '');
            port_internal.setAttributeNS(null, 'r', output_port_internal_size + '');

            out_group.appendChild(port_external);
            out_group.appendChild(port_internal);

            if (this.options.on_io_selected) {
                const element_index = output_index; // Capture for use in callback
                out_group.onclick = ((_ev: MouseEvent) => {
                    this.options.on_io_selected(this, 'out', element_index, output,
                                                { x: port_x_center, y: port_y_center });
                });
            }
            this.output_groups[output_index] = out_group;
        }

        let widest_section = min_width;
        widest_section = Math.max(widest_section, this.textBox.getClientRects()[0].width + x_padding * 2);
        widest_section = Math.max(widest_section, input_x_position);
        widest_section = Math.max(widest_section, output_x_position);

        const box_width = widest_section;

        // Center text box
        this.textBox.setAttributeNS(null, 'x', (this.textCorrection.x
                                                + (box_width/2)
                                                - (this.textBox.getClientRects()[0].width/2)) + "");
        this.textBox.setAttributeNS(null, 'y', (this.textBox.getClientRects()[0].height*2 + this.textCorrection.y) + "");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rect.setAttributeNS(null, 'height', box_height + "");

        this.rect.setAttributeNS(null, 'rx', "10px"); // Like border-radius, in px

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        return this.group;
    }

}
