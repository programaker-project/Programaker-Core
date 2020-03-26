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

interface FlowBlockOptions {
    message: string;
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

        const x_padding = 3; // px
        const y_padding = 2; // px

        this.group = document.createElementNS(SvgNS, 'g');
        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.textBox = document.createElementNS(SvgNS, 'text');

        this.group.setAttribute('class', 'flow_node');
        this.textBox.setAttribute('class', 'actionable');
        this.textBox.textContent = this.options.message;
        this.textBox.setAttributeNS(null,'textlength', '100%');

        this.textBox.setAttributeNS(null, 'x', "0");
        this.textBox.setAttributeNS(null, 'y', "0");

        this.node.appendChild(this.rect);
        this.node.appendChild(this.textBox);
        this.group.appendChild(this.node);
        this.canvas.appendChild(this.group);

        this.textCorrection = {
            x: -(this.textBox.getClientRects()[0].left - canvas.getClientRects()[0].left),
            y: -(this.textBox.getClientRects()[0].top - canvas.getClientRects()[0].top)
        };

        this.textBox.setAttributeNS(null, 'x', (x_padding + this.textCorrection.x) + "");
        this.textBox.setAttributeNS(null, 'y', (y_padding + this.textCorrection.y) + "");

        const box_width = (this.textBox.getClientRects()[0].width + x_padding * 2);
        const box_height = (this.textBox.getClientRects()[0].height + y_padding * 2);

        this.rect.setAttributeNS(null,'x', "0");
        this.rect.setAttributeNS(null,'y', "0");
        this.rect.setAttributeNS(null,'width', box_width + "");
        this.rect.setAttributeNS(null,'height', box_height + "");

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

    public draw(block: FlowBlock) {
        block.render(this.canvas, {x: 10, y: 10});
        block.getBodyElement().onmousedown = ((ev: MouseEvent) => {
            console.log("Selected");
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

        const sample = new FlowBlock({
            message: "This is a test block"
        });

        this.draw(sample);
    }
}
