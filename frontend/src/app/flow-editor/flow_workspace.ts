import {
    FlowBlock,
    InputPortDefinition, OutputPortDefinition,
    Area2D, Position2D
} from './flow_block';
import { FlowConnection } from './flow_connection';
import { uuidv4 } from './utils';

import { StreamingFlowBlock } from './streaming_flow_block';
import { AtomicFlowBlock } from './atomic_flow_block';


const SvgNS = "http://www.w3.org/2000/svg";

const CUT_POINT_SEARCH_INCREASES = 10;
const CUT_POINT_SEARCH_SPACING = CUT_POINT_SEARCH_INCREASES;

type ConnectableNode = {
    block: FlowBlock,
    type: 'in'|'out',
    index: number,
};

export class FlowWorkspace {
    public static BuildOn(baseElement: HTMLElement): FlowWorkspace {
        let workspace: FlowWorkspace;
        try {
            workspace = new FlowWorkspace(baseElement);
            workspace.init();
        }
        catch(err) {
            workspace.dispose();

            throw err;
        }

        return workspace;
    }

    public onResize() {
        this.update_top_left();
    }

    private baseElement: HTMLElement;
    private canvas: SVGElement;
    private connection_group: SVGElement;
    private top_left = { x: 0, y: 0 };
    private inv_zoom_level = 1;

    private blocks: {[key: string]: {
        block: FlowBlock,
        connections: string[],
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

        this.init_definitions();
        this.set_events();
    }

    private init_definitions() {
        const pulse_head_color = "#ffcc00";
        const definitions = document.createElementNS(SvgNS, 'defs');
        definitions.innerHTML = `
<marker id='pulse_head' orient='auto' markerWidth='2' markerHeight='4' refX='1' refY='2'>
  <path d='M0,0 V4 L2,2 Z' fill='${pulse_head_color}' />
</marker>
`;

        this.connection_group.appendChild(definitions);
    }

    private set_events() {
        this.canvas.onmousedown = ((ev: MouseEvent) => {
            if (ev.target !== this.canvas) {
                return;
            }

            let last = { x: ev.x, y: ev.y };

            this.canvas.classList.add('dragging');

            this.canvas.onmousemove = ((ev: MouseEvent) => {
                this.top_left.x -= (ev.x - last.x) * this.inv_zoom_level;
                this.top_left.y -= (ev.y - last.y) * this.inv_zoom_level;
                last = { x: ev.x, y: ev.y };

                this.update_top_left();
            });

            this.canvas.onmouseup = (() => {
                this.canvas.classList.remove('dragging');
                this.canvas.onmousemove = null;
                this.canvas.onmouseup = null;
            });
        });

        this.canvas.onwheel = ((ev) => {
            if(!ev.deltaY){
                return; // ???
            }

            ev.preventDefault();
            if (ev.deltaY < 0) { // Scroll "up"
                this.zoom_in();
            }
            else {
                this.zoom_out();
            }
            this.update_top_left();
        });
    }

    private update_top_left() {
        const width = this.baseElement.clientWidth;
        const height = this.baseElement.clientHeight;

        this.canvas.setAttributeNS(null, 'viewBox',
                                   `${this.top_left.x} ${this.top_left.y} ${width * this.inv_zoom_level} ${height * this.inv_zoom_level}`);
    }

    // Max zoom: 0.5
    // Min zoom: 1/10
    // It's easier to manage zoom level with inverses.
    private zoom_in() {
        if (this.inv_zoom_level <= 0.5) {
            this.inv_zoom_level = 0.5;
            return ;
        }
        else if (this.inv_zoom_level > 10) {
            this.inv_zoom_level = 10;
        }
        else if (this.inv_zoom_level <= 1) {
            this.inv_zoom_level -= 0.5;
        }
        else {
            this.inv_zoom_level -= 1;
        }
    }

    private zoom_out() {
        if (this.inv_zoom_level >= 10) {
            this.inv_zoom_level = 10;
            return;
        }
        else if (this.inv_zoom_level < 0.5) {
            this.inv_zoom_level = 0.5;
        }
        else if (this.inv_zoom_level < 1) {
            this.inv_zoom_level += 0.5;
        }
        else {
            this.inv_zoom_level += 1;
        }
    }

    public dispose() {
        this.baseElement.removeChild(this.canvas);
    }

    public draw(block: FlowBlock, position?: {x: number, y: number}) {
        block.render(this.canvas, position? position: {x: 10, y: 10});
        block.getBodyElement().onmousedown = ((ev: MouseEvent) => {
            if (this.current_io_selected) { return; }

            const block_id = this.getBlockId(block);
            let last = {x: ev.x, y: ev.y};
            this.canvas.onmousemove = ((ev: MouseEvent) => {
                const distance = {
                    x: (ev.x - last.x) * this.inv_zoom_level,
                    y: (ev.y - last.y) * this.inv_zoom_level,
                };
                last = {x: ev.x, y: ev.y};

                block.moveBy(distance);

                for (const conn of this.blocks[block_id].connections) {
                    this.updateConnection(conn);
                }
            });
            this.canvas.onmouseup = ((_ev: MouseEvent) => {
                this.canvas.onmousemove = null;
                this.canvas.onmouseup = null;
            });
        });
        const id = uuidv4();
        this.blocks[id] = { block: block, connections: [] };
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

    private drawPath(path: SVGElement,
                     from: {x: number, y: number},
                     to: {x: number, y: number},
                     runway: number,
                     sink_block?: FlowBlock) {
        let curve: string;

        let bezier_curve = (from.y < to.y);
        if (!bezier_curve && sink_block) {
            // Another option: if a sink block was passed and the `from` point
            //  Y position is within the top and bottom of the sink, also use bezier.
            const area = sink_block.getBodyArea();
            bezier_curve = (from.y > area.y) && (from.y < (area.y + area.height));
        }

        if (bezier_curve) { // Just draw a bezier curve
            const bezier_runway = runway * 3; // Compensate smoothing of the runway
            curve = [
                "M", from.x, ",", from.y,
                " C", from.x, ",", from.y + bezier_runway,
                " ", to.x, ",", to.y - bezier_runway,
                " ", to.x, ",", to.y,
            ].join("");
        }
        else { // Draw a linear circuit

            // We just try to find the X point (where the line goes "up").
            // We don't try to find the Y point and instead just use fixed "runways".
            // This makes finding the route simpler and is good enough for now.
            const x_cut_point = this.find_x_cut_point({x: from.x, y: from.y + runway},
                                                      {x: to.x, y: to.y - runway});

            curve = [
                "M", from.x, ",", from.y,
                " L", from.x, ",", from.y + runway,
                " L", x_cut_point, ",", from.y + runway,
                " L", x_cut_point, ",", to.y - runway,
                " L", to.x, ",", to.y - runway,
                " L", to.x, ",", to.y,
            ].join("");
        }

        path.setAttributeNS(null, "d", curve);
        path.setAttributeNS(null, 'fill', 'none');
        path.setAttributeNS(null, 'stroke', 'black');
        path.setAttributeNS(null, 'stroke-width', '1');
    }

    private find_x_cut_point(from: Position2D, to: Position2D): number {
        const occupied_sections: { left: number, right: number }[] = [];

        let top = from, bottom = to;
        if (from.y > to.y) {
            top = to;
            bottom = from;
        }

        for (const block_id of Object.keys(this.blocks)) {
            const body = this.blocks[block_id].block.getBodyArea();
            if (((body.y + body.height) > top.y) && ((body.y < bottom.y))) {
                occupied_sections.push( { left: body.x, right: body.x + body.width } );
            }
        }

        let cut_point = Math.min(from.x, to.x) + CUT_POINT_SEARCH_INCREASES;

        while (true) {
            let increase = CUT_POINT_SEARCH_INCREASES;

            // Valid cut point?
            let safe_point = true;
            for (const section of occupied_sections) {
                // X-axis position (with any Y-value) falls inside the block?
                if ((cut_point > section.left) && (cut_point < section.right)) {
                    increase = section.right - cut_point + CUT_POINT_SEARCH_SPACING;
                    safe_point = false;
                    break;
                }
            }

            if (safe_point) {
                return cut_point;
            }

            cut_point += increase;
        }
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
        if (source_output_type == 'pulse') {
            path.setAttributeNS(null, 'marker-end', 'url(#pulse_head)');
        }
        this.connection_group.appendChild(path);

        source.connections.push(conn.id);
        this.blocks[conn.getSink().block_id].connections.push(conn.id);

        return path;
    }

    updateConnection(connection_id: string) {
        const conn = this.connections[connection_id];
        const runway = 15;

        // Source
        const source = conn.connection.getSource();
        const source_block = this.blocks[source.block_id].block;

        const source_position = this.getBlockRel(source_block, source_block.getPositionOfOutput(source.output_index));

        // Sink
        const sink = conn.connection.getSink();
        const sink_block = this.blocks[sink.block_id].block;

        const connector_with_marker = !!conn.element.getAttributeNS(null, 'marker-end');
        const y_sink_offset = connector_with_marker ? 2 : 0;

        const sink_position = this.getBlockRel(sink_block, sink_block.getPositionOfInput(sink.input_index, connector_with_marker));
        sink_position.y -= y_sink_offset;

        // Draw
        this.drawPath(conn.element, source_position, sink_position, runway, sink_block);
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

        if (node1.block === node2.block) {
            // Let's not do this intentionally, as removing them might be difficult
            // if this is needed, use an intermediate block.
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
        this.canvas.onclick = null;
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
            this.current_selecting_connector.setAttributeNS(null, 'class', 'building connection ' + type_class);
            this.canvas.appendChild(this.current_selecting_connector);

            let runway = 15;
            if (type == 'in') {
                runway = -runway;
            }

            this.canvas.onmousemove = ((ev: any) => {
                if (!this.canvas.contains(ev.target) || this.isPositionDistoredByFilter(ev)) {
                    return;
                }

                if (type == 'out') {
                    this.drawPath(this.current_selecting_connector,
                                  real_center,
                                  {x: ev.layerX, y: ev.layerY},
                                  runway);
                }
                else {
                    this.drawPath(this.current_selecting_connector,
                                  {x: ev.layerX, y: ev.layerY},
                                  real_center,
                                  runway, block);
                }
            });

            this.canvas.onclick = ((ev: any) => {
                if (ev.target === this.canvas) {
                    this.disconnectIOSelected();
                }
            });
        }
        else {
            if (this.establishConnection(this.current_io_selected,
                                         { block, type, index })){
                this.disconnectIOSelected();
            }
        }
    }

    private draw_addition_sample() {
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

    private draw_message_sample() {
        const on_message = new AtomicFlowBlock({
            title: "Receive message",
            type: "trigger",
            message: "On new message",
            outputs: [
                {
                    name: "message",
                    type: "string",
                },
                {
                    name: "chat",
                    type: "any"
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const send_message = new AtomicFlowBlock({
            title: "Send message",
            type: "operation",
            message: "Send message to chat",
            inputs: [
                {
                    name: "message",
                    type: "string",
                },
                {
                    name: "chat",
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const join = new AtomicFlowBlock({
            message: "Join",
            type: 'operation',
            inputs: [
                {
                    type: "string",
                },
                {
                    type: "string",
                },
            ],
            outputs: [
                {
                    type: 'string',
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const atomic_log = new AtomicFlowBlock({
            message: "Log",
            type: 'operation',
            inputs: [
                {
                    type: "any",
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        this.draw(on_message, { x: 300, y: 10 });
        this.draw(atomic_log, { x: 130, y: 200 });
        this.draw(join, { x: 280, y: 200 });
        this.draw(send_message, { x: 330, y: 400 });

        this.establishConnection({ block: on_message, index: 0, type: 'out' },
                                 { block: join, index: 0, type: 'in' });

        this.establishConnection({ block: on_message, index: 1, type: 'out' },
                                 { block: join, index: 1, type: 'in' });

        this.establishConnection({ block: on_message, index: 1, type: 'out' },
                                 { block: join, index: 2, type: 'in' });

        this.establishConnection({ block: join, index: 0, type: 'out' },
                                 { block: send_message, index: 0, type: 'in' });

        this.establishConnection({ block: join, index: 1, type: 'out' },
                                 { block: send_message, index: 1, type: 'in' });

        this.establishConnection({ block: on_message, index: 2, type: 'out' },
                                 { block: send_message, index: 2, type: 'in' });
    }

    private draw_automatic_door_sample() {
        const webcam = new StreamingFlowBlock({
            message: "Webcam",
            outputs: [
                {
                    name: "Video",
                    type: "any"
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const face_detection = new AtomicFlowBlock({
            type: "trigger",
            message: "Face detection",
            inputs: [
                {
                    name: "video",
                    type: "any",
                },
            ],
            outputs: [
                {
                    name: 'person id',
                    type: 'string',
                },
                {
                    name: 'is enrolled',
                    type: 'boolean',
                },
                {
                    name: 'image',
                    type: 'any',
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const _if_enrolled = new AtomicFlowBlock({
            message: 'if',
            type: 'operation',
            inputs: [
                {
                    type: "boolean",
                },
            ],
            outputs: [
                {
                    name: 'true',
                    type: 'pulse',
                },
                {
                    name: 'false',
                    type: 'pulse',
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const auto_open = new AtomicFlowBlock({
            message: "Open door",
            type: 'operation',
            on_io_selected: this.onIoSelected.bind(this),
        });

        const atomic_log = new AtomicFlowBlock({
            message: "Log",
            type: 'operation',
            inputs: [
                {
                    type: "any",
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        this.draw(webcam, { x: 220, y: 5 });
        this.draw(face_detection, { x: 220, y: 145 });
        this.draw(_if_enrolled, { x: 220, y: 290 });
        this.draw(auto_open, { x: 180, y: 450 });
        this.draw(atomic_log, { x: 300, y: 450 });

        this.establishConnection({ block: webcam, index: 0, type: 'out' },
                                 { block: face_detection, index: 0, type: 'in' });

        this.establishConnection({ block: face_detection, index: 0, type: 'out' },
                                 { block: _if_enrolled, index: 0, type: 'in' });

        this.establishConnection({ block: face_detection, index: 2, type: 'out' },
                                 { block: _if_enrolled, index: 1, type: 'in' });

        this.establishConnection({ block: _if_enrolled, index: 0, type: 'out' },
                                 { block: auto_open, index: 0, type: 'in' });

        this.establishConnection({ block: _if_enrolled, index: 1, type: 'out' },
                                 { block: atomic_log, index: 0, type: 'in' });

        this.establishConnection({ block: face_detection, index: 3, type: 'out' },
                                 { block: atomic_log, index: 1, type: 'in' });
    }

    private draw_standup_bot_sample() {
        // Time parsing
        const time = new StreamingFlowBlock({
            message: "Time",
            outputs: [
                {
                    name: "week day",
                    type: "string"
                },
                {
                    name: "hour",
                    type: "integer"
                },
                {
                    name: "minute",
                    type: "integer"
                },
                {
                    name: "second",
                    type: "integer"
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const comp_hour = new StreamingFlowBlock({
            message: "If equals",
            inputs: [
                {
                    type: "any"
                },
                { // TODO: Accept text field
                    type: "any"
                },
            ],
            outputs: [
                {
                    type: "boolean",
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        })

        const comp_min_sec = new StreamingFlowBlock({
            message: "If equals",
            // TODO: Make variadic
            inputs: [
                {
                    type: "any"
                },
                {
                    type: "any"
                },
                { // TODO: Accept text field
                    type: "any"
                },
            ],
            outputs: [
                {
                    type: "boolean",
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        })

        const and = new StreamingFlowBlock({
            message: "If all true",
            inputs: [
                {
                    type: "boolean"
                },
                {
                    type: "boolean"
                },
            ],
            outputs: [
                {
                    type: "boolean",
                }
            ],
            on_io_selected: this.onIoSelected.bind(this),
        })

        // Start point
        const when_true = new AtomicFlowBlock({
            message: 'when true',
            type: 'trigger',
            inputs: [
                {
                    type: "boolean",
                },
            ],
            outputs: [
                {
                    type: 'pulse',
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const send_message = new AtomicFlowBlock({
            title: "Send message",
            type: "operation",
            message: "Send message to chat",
            inputs: [
                { // TODO: Text field
                    name: "message",
                    type: "string",
                },
                { // TODO: Dropdown field
                    name: "chat",
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const on_message = new AtomicFlowBlock({
            title: "Receive message",
            type: "trigger",
            message: "On new message",
            outputs: [
                {
                    name: "message",
                    type: "string",
                },
                {
                    name: "chat",
                    type: "any"
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const wait_next_value = new AtomicFlowBlock({
            type: "operation",
            message: "Wait next value",
            inputs: [
                {
                    type: "any",
                },
            ],
            outputs: [
                { // TODO: Dependant on input type
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const set_response = new AtomicFlowBlock({
            type: "operation",
            message: "Set variable (var)", // TODO: Variable name dropdown
            inputs: [
                {
                    name: 'value',
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        // Second response
        const send_message2 = new AtomicFlowBlock({
            title: "Send message",
            type: "operation",
            message: "Send message to chat",
            inputs: [
                { // TODO: Text field
                    name: "message",
                    type: "string",
                },
                { // TODO: Dropdown field
                    name: "chat",
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const on_message2 = new AtomicFlowBlock({
            title: "Receive message",
            type: "trigger",
            message: "On new message",
            outputs: [
                {
                    name: "message",
                    type: "string",
                },
                {
                    name: "chat",
                    type: "any"
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const wait_next_value2 = new AtomicFlowBlock({
            type: "operation",
            message: "Wait next value",
            inputs: [
                {
                    type: "any",
                },
            ],
            outputs: [
                { // TODO: Dependant on input type
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });

        const set_response2 = new AtomicFlowBlock({
            type: "operation",
            message: "Set variable (var)", // TODO: Variable name dropdown
            inputs: [
                {
                    name: 'value',
                    type: "any",
                },
            ],
            on_io_selected: this.onIoSelected.bind(this),
        });


        // Draws
        this.draw(time, { x: 50, y: 5 });

        this.draw(comp_hour, { x: 50, y: 200 });
        this.draw(comp_min_sec, { x: 170, y: 200 });
        this.draw(and, { x: 100, y: 400 });

        this.draw(when_true, { x: 450, y: 50 });
        this.draw(send_message, { x: 450, y: 200 });
        this.draw(on_message, { x: 680, y: 200 });
        this.draw(wait_next_value, { x: 600, y: 360 });
        this.draw(set_response, { x: 600, y: 540 });

        this.draw(send_message2, { x: 1000, y: 200 });
        this.draw(on_message2, { x: 1280, y: 200 });
        this.draw(wait_next_value2, { x: 1200, y: 360 });
        this.draw(set_response2, { x: 1200, y: 540 });

        // Time
        this.establishConnection({ block: time, index: 1, type: 'out' },
                                 { block: comp_hour, index: 0, type: 'in' });

        this.establishConnection({ block: time, index: 2, type: 'out' },
                                 { block: comp_min_sec, index: 0, type: 'in' });

        this.establishConnection({ block: time, index: 3, type: 'out' },
                                 { block: comp_min_sec, index: 1, type: 'in' });


        this.establishConnection({ block: comp_hour, index: 0, type: 'out' },
                                 { block: and, index: 0, type: 'in' });

        this.establishConnection({ block: comp_min_sec, index: 0, type: 'out' },
                                 { block: and, index: 1, type: 'in' });

        // First message
        this.establishConnection({ block: and, index: 0, type: 'out' },
                                 { block: when_true, index: 0, type: 'in' });

        this.establishConnection({ block: when_true, index: 0, type: 'out' },
                                 { block: send_message, index: 0, type: 'in' });

        this.establishConnection({ block: send_message, index: 0, type: 'out' },
                                 { block: wait_next_value, index: 0, type: 'in' });

        this.establishConnection({ block: on_message, index: 1, type: 'out' },
                                 { block: wait_next_value, index: 1, type: 'in' });

        this.establishConnection({ block: wait_next_value, index: 0, type: 'out' },
                                 { block: set_response, index: 0, type: 'in' });

        this.establishConnection({ block: wait_next_value, index: 1, type: 'out' },
                                 { block: set_response, index: 1, type: 'in' });

        // Second message
        this.establishConnection({ block: set_response, index: 0, type: 'out' },
                                 { block: send_message2, index: 0, type: 'in' });

        this.establishConnection({ block: send_message2, index: 0, type: 'out' },
                                 { block: wait_next_value2, index: 0, type: 'in' });

        this.establishConnection({ block: on_message2, index: 1, type: 'out' },
                                 { block: wait_next_value2, index: 1, type: 'in' });

        this.establishConnection({ block: wait_next_value2, index: 0, type: 'out' },
                                 { block: set_response2, index: 0, type: 'in' });

        this.establishConnection({ block: wait_next_value2, index: 1, type: 'out' },
                                 { block: set_response2, index: 1, type: 'in' });
    }

    public drawSample() {
        console.log("Drawing sample on", this);

        // this.draw_addition_sample();
        // this.draw_message_sample();
        // this.draw_automatic_door_sample();
        this.draw_standup_bot_sample();
    }
}
