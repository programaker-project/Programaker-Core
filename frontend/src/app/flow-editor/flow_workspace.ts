const SvgNS = "http://www.w3.org/2000/svg";

function uuidv4() {
    // From https://stackoverflow.com/a/2117523
    // Used to generate unique-in-svg IDs for blocks in workspace
    // It just has to be reasonably unique, impredictability here is just overhead.
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

type MessageType = 'integer' | 'string' | 'any';

interface OutputPortDefinitions {
    type?: MessageType;
    name?: string;
}

interface InputPortDefinitions {
    type?: MessageType;
    name?: string;
}

interface FlowBlockOptions {
    message: string;
    outputs?: OutputPortDefinitions[];
    inputs?: InputPortDefinitions[];
}

export class FlowBlock {
    options: FlowBlockOptions;

    constructor(options: FlowBlockOptions) {
        if (!(options.message)) {
            throw new Error("'message' property is required to create a block");
        }

        this.options = options;
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

    public getBodyElement(): SVGElement {
        if (!this.group) {
            throw Error("Not rendered");
        }

        return this.node;
    }

    public moveBy(distance: {x: number, y: number}) {
        if (!this.group) {
            throw Error("Not rendered");
        }

        this.position.x += distance.x;
        this.position.y += distance.y;
        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)
    }

    public render(canvas: SVGElement, position: {x: number, y: number}): SVGElement {
        this.canvas = canvas;
        this.position = { x: position.x, y: position.y };

        if (this.group) { return this.group }

        const min_width = 100;
        const min_height = 25;

        const x_padding = 5; // px
        const y_padding = 5; // px
        const input_initial_x_position = 5; // px
        const inputs_x_margin = 10; // px
        const input_plating_x_margin = 3; // px

        const output_initial_x_position = 5; // px
        const outputs_x_margin = 10; // px
        const output_plating_x_margin = 3; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.textBox = document.createElementNS(SvgNS, 'text');

        this.group.setAttribute('class', 'flow_node');
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
        for (const input of this.options.inputs || []) {
            const in_group = document.createElementNS(SvgNS, 'g');
            this.group.appendChild(in_group);

            const input_port_size = 50;
            const input_port_real_size = 15;
            const input_port_internal_size = 10;
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

            let type_class = 'unknown_type';
            switch (input.type) {
                case 'integer':
                    type_class = 'integer_port';
                    break;

                case 'string':
                    type_class = 'string_port';
                    break;
            }

            // Draw the input port
            const port_external = document.createElementNS(SvgNS, 'circle');
            port_external.setAttributeNS(null, 'class', 'input external_port ' + type_class);
            port_external.setAttributeNS(null, 'cx', (input_position_start + input_position_end) / 2 + '');
            port_external.setAttributeNS(null, 'cy', '0');
            port_external.setAttributeNS(null, 'r', input_port_real_size + '');

            const port_internal = document.createElementNS(SvgNS, 'circle');
            port_internal.setAttributeNS(null, 'class', 'input internal_port');
            port_internal.setAttributeNS(null, 'cx', (input_position_start + input_position_end) / 2 + '');
            port_internal.setAttributeNS(null, 'cy', '0');
            port_internal.setAttributeNS(null, 'r', input_port_internal_size + '');

            in_group.appendChild(port_external);
            in_group.appendChild(port_internal);

            if (input_x_position < input_x_position) {
                input_x_position += input_port_real_size;
            }
        }

        // Add outputs
        let output_x_position = output_initial_x_position + this.textCorrection.x;
        for (const output of this.options.outputs || []) {
            const in_group = document.createElementNS(SvgNS, 'g');
            this.group.appendChild(in_group);

            const output_port_size = 50;
            const output_port_real_size = 15;
            const output_port_internal_size = 10;
            const output_position_start = output_x_position;
            let output_position_end = output_x_position + output_port_size;


            if (output.name) {
                // Bind output name and port
                const port_plating = document.createElementNS(SvgNS, 'rect');
                in_group.appendChild(port_plating);

                const text = document.createElementNS(SvgNS, 'text');
                text.textContent = output.name;
                text.setAttributeNS(null, 'class', 'argument_name output');
                in_group.appendChild(text);

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

            let type_class = 'unknown_type';
            switch (output.type) {
                case 'integer':
                    type_class = 'integer_port';
                    break;

                case 'string':
                    type_class = 'string_port';
                    break;
            }

            // Draw the output port
            const port_external = document.createElementNS(SvgNS, 'circle');
            port_external.setAttributeNS(null, 'class', 'output external_port ' + type_class);
            port_external.setAttributeNS(null, 'cx', (output_position_start + output_position_end) / 2 + '');
            port_external.setAttributeNS(null, 'cy', box_height + '');
            port_external.setAttributeNS(null, 'r', output_port_real_size + '');

            const port_internal = document.createElementNS(SvgNS, 'circle');
            port_internal.setAttributeNS(null, 'class', 'output internal_port');
            port_internal.setAttributeNS(null, 'cx', (output_position_start + output_position_end) / 2 + '');
            port_internal.setAttributeNS(null, 'cy', box_height + '');
            port_internal.setAttributeNS(null, 'r', output_port_internal_size + '');

            in_group.appendChild(port_external);
            in_group.appendChild(port_internal);

            if (output_x_position < output_x_position) {
                output_x_position += output_port_real_size;
            }
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

        this.rect.setAttributeNS(null, 'rx', "5px"); // Like border-radius, in px

        this.group.setAttribute('transform', `translate(${this.position.x}, ${this.position.y})`)

        return this.group;
    }

}

export class FlowWorkspace {
    public static BuildOn(baseElement: HTMLElement): FlowWorkspace {
        const workspace = new FlowWorkspace(baseElement);

        try {
            workspace.init();
        }
        catch(err) {
            workspace.dispose();

            throw err;
        }

        return workspace;
    }

    private baseElement: HTMLElement;
    private canvas: SVGElement;
    private blocks: {[key: string]: {
        block: FlowBlock,
    }};

    private constructor(baseElement: HTMLElement) {
        this.baseElement = baseElement;
        this.blocks = {};
    }

    private init() {
        this.canvas = document.createElementNS(SvgNS, "svg");
        this.baseElement.appendChild(this.canvas);
    }

    public dispose() {
        this.baseElement.removeChild(this.canvas);
    }

    public draw(block: FlowBlock, position?: {x: number, y: number}) {
        block.render(this.canvas, position? position: {x: 10, y: 10});
        block.getBodyElement().onmousedown = ((ev: MouseEvent) => {
            let last = {x: ev.x, y: ev.y};
            this.canvas.onmousemove = ((ev: MouseEvent) => {
                const distance = { x: ev.x - last.x, y: ev.y - last.y };
                last = {x: ev.x, y: ev.y};

                block.moveBy(distance);
            });
            this.canvas.onmouseup = ((_ev: MouseEvent) => {
                this.canvas.onmousemove = null;
                this.canvas.onmouseup = null;
            });
        });
        const id = uuidv4();
        this.blocks[id] = { block: block };
    }

    public removeBlock(blockId: string) {
        const info = this.blocks[blockId];
        info.block.dispose();
    }

    public drawSample() {
        console.log("Drawing sample on", this);

        const number1 = new FlowBlock({
            message: "Number",
            outputs: [{
                type: "integer",
                name: "number"
            }],
        });

        const number2 = new FlowBlock({
            message: "Number",
            outputs: [{
                type: "integer",
                name: "number"
            }],
        });

        const add = new FlowBlock({
            message: "Add",
            inputs: [
                {
                    type: "integer",
                    name: "number",
                },
                {
                    type: "integer",
                    name: "number",
                }],
            outputs: [
                {
                    type: "integer",
                    name: "result",
                }
            ]
        });

        const log = new FlowBlock({
            message: "Log",
            inputs: [
                {
                    type: "any",
                }
            ],
            outputs: [
                {
                    type: "any",
                }
            ]
        });

        this.draw(number1, {x:  30, y: 30});
        this.draw(number2, {x: 230, y: 30});
        this.draw(add, {x: 130, y: 230});
        this.draw(log, {x: 130, y: 430});
    }
}
