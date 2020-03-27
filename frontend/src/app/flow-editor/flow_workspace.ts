import { FlowBlock, InputPortDefinition, OutputPortDefinition } from './flow_block';
import { StreamingFlowBlock } from './streaming_flow_block';
import { FlowConnection } from './flow_connection';
import { uuidv4 } from './utils';

const SvgNS = "http://www.w3.org/2000/svg";

type ConnectableNode = {
    block: FlowBlock,
    type: 'in'|'out',
    index: number,
    definition: InputPortDefinition | OutputPortDefinition,
};

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

    private connection_group: SVGElement;
    private baseElement: HTMLElement;
    private canvas: SVGElement;
    private blocks: {[key: string]: {
        block: FlowBlock,
    }};
    private connections: {[key: string]: {
        connection: FlowConnection,
        element: SVGElement,
    }};

    private constructor(baseElement: HTMLElement) {
        this.baseElement = baseElement;
        this.blocks = {};
        this.connections = {};
    }

    private init() {
        this.canvas = document.createElementNS(SvgNS, "svg");
        this.connection_group = document.createElementNS(SvgNS, "g");

        this.canvas.appendChild(this.connection_group);
        this.baseElement.appendChild(this.canvas);
    }

    public dispose() {
        this.baseElement.removeChild(this.canvas);
    }

    public draw(block: FlowBlock, position?: {x: number, y: number}) {
        block.render(this.canvas, position? position: {x: 10, y: 10});
        block.getBodyElement().onmousedown = ((ev: MouseEvent) => {
            if (this.current_io_selected) { return; }

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

    private getBlockRel(block: FlowBlock, position: {x: number, y: number}): { x: number, y: number }{
        const off = block.getOffset();
        return { x: off.x + position.x, y: off.y + position.y };
    }

    private current_io_selected: {
        block: FlowBlock,
        type: 'in'|'out',
        index: number,
        definition: InputPortDefinition | OutputPortDefinition,
        port_center: {x: number, y: number},
        real_center: {x: number, y: number}
    };
    private current_selecting_connector: SVGElement;

    private drawPath(path: SVGElement, from: {x: number, y: number}, to: {x: number, y: number}, runway: number) {
        const curve = [
            "M", from.x, ",", from.y,
            " C", from.x, ",", from.y + runway,
            " ", to.x, ",", to.y - runway,
            " ", to.x, ",", to.y,
        ].join("");

        path.setAttributeNS(null, "d", curve);
        path.setAttributeNS(null, 'fill', 'none');
        path.setAttributeNS(null, 'stroke', 'black');
        path.setAttributeNS(null, 'stroke-width', '1');

    }

    private isPositionDistoredByFilter(ev: MouseEvent): boolean {
        // An ad-hoc heuristic to detect when a CSS filter might
        // have distorted the ev.layer values.
        // The `drop-shadow` filter does this.
        //
        // @TODO: Get a better measure
        const target = ev.target as any;
        if (target.tagName === "rect"){
            const top_attr = target.getAttributeNS(null, 'y');
            if (top_attr !== undefined) {
                const top = parseInt(top_attr);

                if ((ev as any).layerY < top) {
                    return true;
                }
            }
        }

        return false;
    }

    // TODO: Remove this
    // private getTranslation(element: SVGElement): {x: number, y: number} {
    //     const translation = {x: 0, y: 0};
    //     while ((element) && (element !== this.canvas)) {
    //         if ((element as any).transform) {
    //             const base = (element as any).transform.baseVal; // An SVGTransformList
    //             for (const val of base) {
    //                 if (val.type == SVGTransform.SVG_TRANSFORM_TRANSLATE){
    //                     translation.x += val.matrix.e,
    //                     translation.y += val.matrix.f;
    //                 }
    //             }
    //         }

    //         element = element.parentElement as any as SVGElement;
    //     }

    //     return translation;
    // }

    addConnection(conn: FlowConnection): boolean {
        const element = this.prepareConnection(conn);

        if (!element) {
            return false;
        }
        this.connections[conn.id] = { connection: conn, element: element };

        this.updateConnection(conn.getId());

        return true;
    }

    prepareConnection(conn: FlowConnection): SVGElement {
        const source = this.blocks[conn.getSource().block_id];
        const source_output_type = source.block.getOutputType(conn.getSource().output_index);

        let type_class = "unknown_wire";
        if (source_output_type) {
            type_class = source_output_type + '_wire';
        }

        const path = document.createElementNS(SvgNS, 'path');
        path.setAttributeNS(null, 'class', 'established connection ' + type_class);
        this.connection_group.appendChild(path);

        return path;
    }

    updateConnection(connection_id: string) {
        const conn = this.connections[connection_id];

        const runway = 50;
        const source = conn.connection.getSource();
        const source_block = this.blocks[source.block_id].block;

        const sink = conn.connection.getSink();
        const sink_block = this.blocks[sink.block_id].block;

        this.drawPath(conn.element,
                      this.getBlockRel(source_block, source_block.getPositionOfOutput(source.output_index)),
                      this.getBlockRel(sink_block, sink_block.getPositionOfInput(sink.input_index)),
                      runway,
                     );
    }

    getBlockId(block: FlowBlock): string {
        for (const key of Object.keys(this.blocks)) {
            if (this.blocks[key].block === block) {
                return key;
            }
        }

        return null;
    }

    establishConnection(node1: ConnectableNode, node2: ConnectableNode): boolean {
        if ((node1.type === node2.type)) { // An input and an output is required
            return false;
        }

        let source = node2;
        let sink = node1;
        if (node1.type === 'out') {
            source = node1;
            sink = node2;
        }

        const connection = new FlowConnection({block_id: this.getBlockId(source.block), output_index: source.index },
                                              {block_id: this.getBlockId(sink.block), input_index: sink.index },
                                             );
        return this.addConnection(connection);
    }

    private disconnectIOSelected() {
        this.canvas.removeChild(this.current_selecting_connector);
        this.current_selecting_connector = null;
        this.current_io_selected = null;

        this.canvas.onmousemove = null;
        this.canvas.onmousedown = null;
    }

    private onIoSelected(block: FlowBlock,
                           type: 'in'|'out',
                           index: number,
                           definition: InputPortDefinition | OutputPortDefinition,
                           port_center: {x: number, y: number},
                        ): void {

        if (!this.current_io_selected) {
            const real_center = this.getBlockRel(block, port_center);
            this.current_io_selected = { block, type, index, definition, port_center, real_center };


            let type_class = "unknown_wire";
            if (definition.type) {
                type_class = definition.type + '_wire';
            }

            this.current_selecting_connector = document.createElementNS(SvgNS, 'path');
            this.current_selecting_connector.setAttributeNS(null, 'class', 'building connector ' + type_class);
            this.canvas.appendChild(this.current_selecting_connector);

            let runway = 50;
            if (type == 'in') {
                runway = -runway;
            }

            this.canvas.onmousemove = ((ev: any) => {
                if (!this.canvas.contains(ev.target) || this.isPositionDistoredByFilter(ev)) {
                    return;
                }

                let correction = {x: 0, y: 0};
                // This has to be applied, but only if the ev.target has a filter...
                // if (ev.target.tagName.toLowerCase() === 'rect') {
                //     const trans = this.getTranslation(ev.target);
                //     correction = trans;
                // }

                this.drawPath(this.current_selecting_connector, real_center,
                              {x: ev.layerX + correction.x, y: ev.layerY + correction.y},
                              runway);
            });

            this.canvas.onmousedown = ((ev: any) => {
                if (ev.target === this.canvas) {
                    this.disconnectIOSelected();
                }
            });
        }
        else {
            if (this.establishConnection(this.current_io_selected,
                                          { block,
                                            type,
                                            index,
                                            definition,
                                          },
                                         )){
                this.disconnectIOSelected();
            }

        }
    }

    public drawSample() {
        console.log("Drawing sample on", this);

        const number1 = new StreamingFlowBlock({
            message: "Number",
            outputs: [{
                type: "integer",
                name: "number"
            }],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const number2 = new StreamingFlowBlock({
            message: "Number",
            outputs: [{
                type: "integer",
                name: "number"
            }],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const add = new StreamingFlowBlock({
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
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const log = new StreamingFlowBlock({
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
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        this.draw(number1, {x:  30, y: 30});
        this.draw(number2, {x: 230, y: 30});
        this.draw(add, {x: 130, y: 230});
        this.draw(log, {x: 130, y: 430});
    }
}
