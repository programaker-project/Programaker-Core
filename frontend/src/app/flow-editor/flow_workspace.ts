import { BlockManager } from './block_manager';
import { DirectValue } from './direct_value';
import { EnumValue, EnumDirectValue, EnumGetter } from './enum_direct_value';
import { Area2D, Direction2D, FlowBlock, InputPortDefinition, MessageType, OutputPortDefinition, Position2D, BridgeEnumInputPortDefinition, Resizeable, ContainerBlock } from './flow_block';
import { FlowConnection } from './flow_connection';
import { uuidv4 } from './utils';
import { FlowGraph, FlowGraphNode, FlowGraphEdge } from './flow_graph';
import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { UiFlowBlockData, UiFlowBlock } from './ui-blocks/ui_flow_block';
import { Toolbox } from './toolbox';
import { ContainerFlowBlock, isContainerFlowBlockOptions, ContainerFlowBlockData, isContainerFlowBlockData } from './ui-blocks/container_flow_block';

/// <reference path="../../../node_modules/fuse.js/dist/fuse.d.ts" />
declare const Fuse: any;

const SvgNS = "http://www.w3.org/2000/svg";

const INV_MAX_ZOOM_LEVEL = 5;

const CUT_POINT_SEARCH_INCREASES = 10;
const CUT_POINT_SEARCH_SPACING = CUT_POINT_SEARCH_INCREASES;

// Draw helper
const HELPER_BASE_SIZE = 25;
const HELPER_SEPARATION = HELPER_BASE_SIZE * 1.5;


type ConnectableNode = {
    block: FlowBlock,
    type: 'in'|'out',
    index: number,
};

type State = 'waiting'     // Base state
    | 'dragging-block'     // Moving around a block
    | 'dragging-workspace' // Moving around the workspace


export class FlowWorkspace implements BlockManager {
    public static BuildOn(baseElement: HTMLElement,
                          getEnum: EnumGetter): FlowWorkspace {
        let workspace: FlowWorkspace;
        try {
            workspace = new FlowWorkspace(baseElement, getEnum);
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

    public getCanvas(): SVGSVGElement {
        return this.canvas;
    }

    public getGraph(): FlowGraph {
        const blocks: { [key: string]: FlowGraphNode } = {};
        for (const block_id of Object.keys(this.blocks)) {
            const block = this.blocks[block_id].block;
            const serialized = block.serialize();
            const position = block.getOffset();

            blocks[block_id] = { data: serialized, position: position, container_id: this.blocks[block_id].container_id };
        }

        const connections: FlowGraphEdge[] = [];

        for (const conn_id of Object.keys(this.connections)) {
            const connection = this.connections[conn_id].connection;

            const source = connection.getSource();
            const sink = connection.getSink();
            connections.push({
                from: { id: source.block_id, output_index: source.output_index },
                to: { id: sink.block_id, input_index: sink.input_index },
            });
        }

        return {
            nodes: blocks,
            edges: connections,
        }
    }

    public load(graph: FlowGraph, toolbox: Toolbox) {
        let to_go = Object.keys(graph.nodes);

        let processing = true;

        while ((to_go.length > 0) && processing) {
            processing = false;
            const skipped = [];

            for (const block_id of to_go) {
                const block = graph.nodes[block_id];
                if (block.container_id && (!this.blocks[block.container_id])) {
                    skipped.push(block_id);
                    continue;
                }

                let created_block = null;
                switch (block.data.type) {
                    case AtomicFlowBlock.GetBlockType():
                        created_block = AtomicFlowBlock.Deserialize(block.data as AtomicFlowBlockData, this);
                        break;

                    case UiFlowBlock.GetBlockType():
                        if (isContainerFlowBlockData(block.data)) {
                            created_block = ContainerFlowBlock.Deserialize(block.data as ContainerFlowBlockData, this, toolbox);
                        }
                        else {
                            created_block = UiFlowBlock.Deserialize(block.data as UiFlowBlockData, this, toolbox);
                        }
                        break;

                    case DirectValue.GetBlockType():
                        created_block = DirectValue.Deserialize(block.data, this);
                        break;

                    case EnumDirectValue.GetBlockType():
                        created_block = EnumDirectValue.Deserialize(block.data, this, this.getEnum);
                        break;

                    default:
                        console.error("Unknown block type:", block.data.type);
                }

                if (!created_block) {
                    console.error("Error deserializing block:", block.data);
                    continue;
                }

                try {
                    this.draw(created_block, block.position, block_id);
                }
                catch (err) {
                    console.error("Error drawing block", err);
                }

                if (block.container_id) {
                    this._updateBlockContainer(created_block, this.blocks[block.container_id].block);
                }
                processing = true;
            }

            to_go = skipped;
        }

        if (to_go.length !== 0) {
            throw new Error("Found container-contained circular dependency, on the following IDs: " + JSON.stringify(to_go));
        }

        for (const conn of graph.edges) {
            try {
                this.establishConnection(
                    {
                        block: this.blocks[conn.from.id].block,
                        type: 'out',
                        index: conn.from.output_index,
                    },
                    {
                        block: this.blocks[conn.to.id].block,
                        type: 'in',
                        index: conn.to.input_index,
                    },
                )
            }
            catch(err) {
                console.error("Error establishing connection", err);
            }
        }
    }

    private baseElement: HTMLElement;
    private inlineEditorContainer: HTMLDivElement;
    private inlineEditor: HTMLInputElement;
    private state: State = 'waiting';

    private popupGroup: HTMLDivElement;
    private canvas: SVGSVGElement;
    private connection_group: SVGGElement;
    private block_group: SVGGElement;
    private container_group: SVGGElement;
    private containers: FlowBlock[] = [];

    private top_left = { x: 0, y: 0 };
    private inv_zoom_level = 1;
    private input_helper_section: SVGGElement;
    private trashcan: SVGGElement;
    private variables_in_use: { [key: string]: number } = {};
    private getEnum: EnumGetter;

    private blocks: {[key: string]: {
        block: FlowBlock,
        connections: string[],
        input_group: SVGGElement,
        container_id: string | null,
    }};

    private connections: {[key: string]: {
        connection: FlowConnection,
        element: SVGElement,
    }};

    private constructor(baseElement: HTMLElement, getEnum: EnumGetter) {
        this.baseElement = baseElement;
        this.inlineEditor = undefined;
        this.blocks = {};
        this.connections = {};
        this.getEnum = getEnum;
    }

    private init() {
        // Inline editor
        this.inlineEditorContainer = document.createElement('div');
        this.inlineEditorContainer.setAttribute('class', 'inline_editor_container hidden');
        this.baseElement.appendChild(this.inlineEditorContainer);
        this.inlineEditor = document.createElement('input');
        this.inlineEditorContainer.appendChild(this.inlineEditor);

        // Popup group
        this.popupGroup = document.createElement('div');
        this.popupGroup.setAttribute('class', 'popup_group hidden');
        this.baseElement.appendChild(this.popupGroup);

        this.canvas = document.createElementNS(SvgNS, "svg");
        this.canvas.setAttribute('class', 'block_renderer');

        this.input_helper_section = document.createElementNS(SvgNS, "g");
        this.trashcan = document.createElementNS(SvgNS, "g");
        this.connection_group = document.createElementNS(SvgNS, "g");
        this.block_group = document.createElementNS(SvgNS, 'g');
        this.container_group = document.createElementNS(SvgNS, 'g');

        // The order of elements determines the relative Z-index
        // The "later" an element is added, the "higher" it is.
        // The elements are stored in groups so their Z-indexes are consistent.
        this.canvas.appendChild(this.container_group);
        this.canvas.appendChild(this.input_helper_section);
        this.canvas.appendChild(this.trashcan);
        this.canvas.appendChild(this.connection_group);
        this.canvas.appendChild(this.block_group);
        this.baseElement.appendChild(this.canvas);

        this.init_definitions();
        this.set_events();
        this.init_trashcan();

        this.update_top_left();
    }

    private init_definitions() {
        const pulse_head_color = "#ffcc00";
        const pulse_head_selected_color = "#bf8c00";

        const definitions = document.createElementNS(SvgNS, 'defs');
        definitions.innerHTML = `
<marker id='pulse_head' orient='auto' markerWidth='2' markerHeight='4' refX='1' refY='2'>
  <path d='M0,0 V4 L2,2 Z' fill='${pulse_head_color}' />
</marker>
<marker id='pulse_head_selected' orient='auto' markerWidth='2' markerHeight='4' refX='1' refY='2'>
  <path d='M0,0 V4 L2,2 Z' fill='${pulse_head_selected_color}' />
</marker>
<filter id="shadow" x="-50%" y="-50%" width="200%" height="200%">
  <feColorMatrix in="SourceGraphic" out="inverted" type="matrix" values="
1 0 0 0 0
0 1 0 0 0
0 0 1 0 0
0 0 0 1 0
"></feColorMatrix>
  <feGaussianBlur result="blurOut" in="inverted" stdDeviation="2"></feGaussianBlur>
  <feBlend in="SourceGraphic" in2="blurOut" mode="normal"></feBlend>
</filter>
`;

        this.connection_group.appendChild(definitions);
    }

    private init_trashcan() {
        this.trashcan.setAttribute('class', 'trashcan helper');

        const rect = document.createElementNS(SvgNS, 'rect');
        rect.setAttributeNS(null, 'class', 'backdrop');
        rect.setAttributeNS(null, 'width', '5ex');
        rect.setAttributeNS(null, 'height', '8ex');

        const shadow = document.createElementNS(SvgNS, 'rect');
        shadow.setAttributeNS(null, 'class', 'backdrop_shadow');
        shadow.setAttributeNS(null, 'width', '5ex');
        shadow.setAttributeNS(null, 'height', '8ex');

        const image = document.createElementNS(SvgNS, 'image');
        image.setAttributeNS(null, 'href', '/assets/sprites/delete_forever-black.svg');
        image.setAttributeNS(null, 'width', '5ex');
        image.setAttributeNS(null, 'height', '8ex');

        this.trashcan.appendChild(shadow);
        this.trashcan.appendChild(rect);
        this.trashcan.appendChild(image);
    }

    private set_events() {
        this.canvas.onmousedown = ((ev: MouseEvent) => {
            if (this.state !== 'waiting') {
                return;
            }

            if (ev.target !== this.canvas) {
                return;
            }

            let last = { x: ev.x, y: ev.y };

            this.state = 'dragging-workspace';
            this.canvas.classList.add('dragging');

            this.canvas.onmousemove = ((ev: MouseEvent) => {
                this.top_left.x -= (ev.x - last.x) * this.inv_zoom_level;
                this.top_left.y -= (ev.y - last.y) * this.inv_zoom_level;
                last = { x: ev.x, y: ev.y };

                this.update_top_left();
            });

            this.canvas.onmouseup = (() => {
                this.state = 'waiting';
                this.canvas.classList.remove('dragging');
                this.canvas.onmousemove = null;
                this.canvas.onmouseup = null;
            });
        });

        this.canvas.ontouchstart = ((ev: TouchEvent) => {
            if (this.state !== 'waiting') {
                return;
            }

            if (ev.target !== this.canvas) {
                return;
            }

            const touch = ev.targetTouches[0];
            let last = { x: touch.clientX, y: touch.clientY };

            this.state = 'dragging-workspace';
            this.canvas.classList.add('dragging');

            this.canvas.ontouchmove = ((ev: TouchEvent) => {
                if (ev.targetTouches.length == 0) {
                    return;
                }
                if (ev.targetTouches.length == 1) {
                    const touch = ev.targetTouches[0];
                    this.top_left.x -= (touch.clientX - last.x) * this.inv_zoom_level;
                    this.top_left.y -= (touch.clientY - last.y) * this.inv_zoom_level;
                    last = { x: touch.clientX, y: touch.clientY };

                    this.update_top_left();
                }
                else {
                    console.log("Unexpected action with more than one touch", ev);
                }
            });

            this.canvas.ontouchend = (() => {
                this.state = 'waiting';
                this.canvas.classList.remove('dragging');
                this.canvas.ontouchmove = null;
                this.canvas.ontouchend = null;
            });
        })

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

        this.canvas.style.backgroundPosition = `${-this.top_left.x}px ${-this.top_left.y}px`;

        // Move trashcan
        const trashbox = this.trashcan.getElementsByTagName('image')[0].getBBox();
        if (trashbox) {
            const left = width * this.inv_zoom_level - trashbox.width + this.top_left.x;
            const top = height * this.inv_zoom_level - trashbox.height + this.top_left.y;
            this.trashcan.setAttributeNS(null, 'transform', `translate(${left},${top})`);
        }
    }

    // Max zoom: 0.5
    // Min zoom: 1/10
    // It's easier to manage zoom level with inverses.
    private zoom_in() {
        if (this.inv_zoom_level <= 0.5) {
            this.inv_zoom_level = 0.5;
            return ;
        }
        else if (this.inv_zoom_level > INV_MAX_ZOOM_LEVEL) {
            this.inv_zoom_level = INV_MAX_ZOOM_LEVEL;
        }
        else if (this.inv_zoom_level <= 1) {
            this.inv_zoom_level -= 0.5;
        }
        else {
            this.inv_zoom_level -= 1;
        }
    }

    private zoom_out() {
        if (this.inv_zoom_level >= INV_MAX_ZOOM_LEVEL) {
            this.inv_zoom_level = INV_MAX_ZOOM_LEVEL;
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
        if (this.inlineEditorContainer) {
            this.baseElement.removeChild(this.inlineEditorContainer);
            this.inlineEditorContainer = null;
        }

        if (this.popupGroup) {
            this.baseElement.removeChild(this.popupGroup);
            this.popupGroup = null;
        }

        if (this.canvas) {
            this.baseElement.removeChild(this.canvas);
            this.canvas = null;
        }
    }

    public drawAbsolute(block: FlowBlock, abs_position: Position2D): string {
        const canvas_area = this.canvas.getClientRects()[0];

        const rel_pos = {
            x: abs_position.x - canvas_area.x + this.top_left.x,
            y: abs_position.y - canvas_area.y + this.top_left.y,
        };

        return this.draw(block, rel_pos);
    }

    public draw(block: FlowBlock, position?: Position2D, block_id?: string): string {
        if (block_id) {
            if (this.blocks[block_id]) {
                throw new Error('Duplicated block id');
            }
        }
        else {
            block_id = uuidv4();
        }

        const slots = block.getSlots();
        if (slots.variable) {
            if (!this.variables_in_use[slots.variable]) {
                this.variables_in_use[slots.variable] = 0;
            }
            this.variables_in_use[slots.variable]++;
        }

        let group = this.block_group;
        const isContainer = block instanceof ContainerFlowBlock;
        if (isContainer) {
            group = this.container_group;
        }
        block.render(group, {
            block_id: block_id,
            position: (position ? position : {x: 10, y: 10}),
            workspace: this,
        });

        if (isContainer) {
            // Obtaining the area has to be done AFTER the rendering
            this.containers.push(block);
        }

        const bodyElement = block.getBodyElement();
        bodyElement.onmousedown = bodyElement.ontouchstart = ((ev: MouseEvent | TouchEvent) => {
            if (this.state !== 'waiting'){
                return;
            }

            if (this.current_io_selected) { return; }

            this._mouseDownOnBlock(this._getPositionFromEvent(ev), block);
        });
        const input_group = this.drawBlockInputHelpers(block);

        this.blocks[block_id] = { block: block, connections: [], input_group: input_group, container_id: null };

        return block_id;
    }

    public _getPositionFromEvent(ev: MouseEvent | TouchEvent) : Position2D | null {
        if ((ev as TouchEvent).targetTouches) {
            const touchEv = ev as TouchEvent;
            if (touchEv.targetTouches.length === 0) {
                return null;
            }
            return { x: touchEv.targetTouches[0].clientX, y: touchEv.targetTouches[0].clientY };
        }
        else {
            const mouseEv = ev as MouseEvent;
            return { x: mouseEv.clientX, y: mouseEv.clientY };
        }
    }

    public startResizing(block: Resizeable, ev: MouseEvent | TouchEvent) {
        const initialPos = this._getPositionFromEvent(ev);
        const area = block.getBodyArea();

        this.canvas.onmousemove = this.canvas.ontouchmove = ((ev: MouseEvent | TouchEvent) => {
            const pos = this._getPositionFromEvent(ev);

            const diffX = initialPos.x - pos.x;
            const diffY = initialPos.y - pos.y;

            const newWidth = area.width - diffX;
            const newHeight = area.height - diffY;

            block.resize({ width: newWidth, height: newHeight });
        });

        this.canvas.onmouseup = this.canvas.ontouchend = ((ev: MouseEvent | TouchEvent) => {
            this.canvas.onmousemove = null;
            this.canvas.onmouseup = null;
        });
    }

    private _findContainerInPos(pos: Position2D, excluding?: FlowBlock): FlowBlock | null {
        for (const container of this.containers) {
            if (excluding === container) {
                continue;
            }

            const area = container.getBodyElement().getClientRects()[0];
            if (!area) {
                continue;
            }

            const diffX = pos.x - area.x;
            const diffY = pos.y - area.y;

            if ((diffX >= 0) && (diffY >= 0)
                && (diffX <= area.width)
                && (diffY <= area.height)) {

                return container;
            }
        }

        return null;
    }

    private _getContainerOfBlock(blockId: string): FlowBlock | null {
        const blockInfo = this.blocks[blockId];
        if (blockInfo.container_id) {
            return this.blocks[blockInfo.container_id].block;
        }

        return null;
    }

    private _updateBlockContainer(block: FlowBlock, container?: FlowBlock) {
        const block_id = this.getBlockId(block);
        const wasInContainer = this._getContainerOfBlock(block_id);

        const container_id = container ? this.getBlockId(container) : null;

        if (wasInContainer !== container) {
            if (wasInContainer) {
                (wasInContainer as any as ContainerBlock).removeContentBlock(block);
            }

            if (container) {
                (container as any as ContainerBlock).addContentBlock(block);
            }
            this.blocks[block_id].container_id = container_id;
        }
        else if (container) {
            (container as any as ContainerBlock).update();
        }
    }

    private _mouseDownOnBlock(pos: Position2D, block: FlowBlock, on_done?: (pos: Position2D) => void) {
        if (this.state !== 'waiting') {
            console.error('Forcing start of MouseDown with Workspace state='+this.state);
        }
        this.state = 'dragging-block';

        const bodyElement = block.getBodyElement();
        const block_id = this.getBlockId(block);

        let last = pos;
        let lastContainer: FlowBlock | null = null;

        this.canvas.onmousemove = this.canvas.ontouchmove = ((ev: MouseEvent | TouchEvent) => {
            const pos = this._getPositionFromEvent(ev);
            const container = this._findContainerInPos(pos, block);

            if (lastContainer !== container) {
                if (lastContainer) {
                    lastContainer.getBodyElement().classList.remove('highlighted');
                }

                if (container) {
                    container.getBodyElement().classList.add('highlighted');
                }

                lastContainer = container;
            }



            try {
                const distance = {
                    x: (pos.x - last.x) * this.inv_zoom_level,
                    y: (pos.y - last.y) * this.inv_zoom_level,
                };
                last = {x: pos.x, y: pos.y};

                block.moveBy(distance);

                for (const conn of this.blocks[block_id].connections) {
                    this.updateConnection(conn);
                }
                this.updateBlockInputHelpersPosition(block_id);

                if (this.isInTrashcan(pos)) {
                    this.trashcan.classList.add('to-be-activated');
                    bodyElement.classList.add('to-be-removed');
                }
                else {
                    this.trashcan.classList.remove('to-be-activated');
                    bodyElement.classList.remove('to-be-removed');
                }
            }
            catch(err) {
                console.error(err);
            }
        });
        this.canvas.onmouseup = this.canvas.ontouchend = ((ev: MouseEvent | TouchEvent) => {
            if (lastContainer) {
                lastContainer.getBodyElement().classList.remove('highlighted');
            }

            const pos = this._getPositionFromEvent(ev);
            const container = this._findContainerInPos(pos, block);
            this._updateBlockContainer(block, container);

            try {
                const pos = this._getPositionFromEvent(ev) || last;

                if (on_done) {
                    on_done(pos);
                }

                this.state = 'waiting';
                this.canvas.onmousemove = null;
                this.canvas.onmouseup = null;
                this.trashcan.classList.remove('to-be-activated');
                bodyElement.classList.remove('to-be-removed');

                if (this.isInTrashcan(pos)) {
                    this.removeBlock(block_id);
                }
            }
            catch (err) {
                console.error(err);
            }
        });
    }

    private isInTrashcan(pos: Position2D): boolean {
        const rect = this.trashcan.getElementsByTagName('image')[0].getClientRects()[0];
        if ((rect.x <= pos.x) && (rect.x + rect.width >= pos.x)) {
            if ((rect.y <= pos.y) && (rect.y + rect.height >= pos.y)) {
                return true;
            }
        }

        return false;
    }

    private drawBlockInputHelpers(block: FlowBlock, inputHelperGroup?: SVGGElement): SVGGElement {
        let existing_inputs = 0;

        if (!inputHelperGroup) {
            inputHelperGroup = document.createElementNS(SvgNS, 'g');
        }
        else{
            existing_inputs = inputHelperGroup.children.length;
        }

        let index = -1;
        for (const input of block.getInputs()) {
            index++;

            if (index < existing_inputs) {
                continue; // Don't re-draw existing inputs
            }

            const inputGroup = document.createElementNS(SvgNS, 'g');
            const input_position = this.getBlockRel(block, block.getPositionOfInput(index));

            let type_class = 'unknown_type';
            if (input.type) {
                type_class = input.type + '_port';
            }

            inputGroup.setAttributeNS(null, 'class', 'input_helper ' + type_class);
            inputHelperGroup.appendChild(inputGroup);

            const outerCircle = document.createElementNS(SvgNS, 'circle');
            const verticalBar = document.createElementNS(SvgNS, 'rect');
            const horizontalBar = document.createElementNS(SvgNS, 'rect');
            const connectionLine = document.createElementNS(SvgNS, 'path');

            outerCircle.setAttributeNS(null, 'class', 'outer_circle');
            outerCircle.setAttributeNS(null, 'cx', HELPER_BASE_SIZE / 2 + '');
            outerCircle.setAttributeNS(null, 'cy', HELPER_BASE_SIZE / 2 + '');
            outerCircle.setAttributeNS(null, 'r', HELPER_BASE_SIZE / 2 + '');

            verticalBar.setAttributeNS(null, 'class', 'vertical_bar');
            verticalBar.setAttributeNS(null, 'x', (HELPER_BASE_SIZE / 5) * 2 + '' );
            verticalBar.setAttributeNS(null, 'y', HELPER_BASE_SIZE / 4 + '' );
            verticalBar.setAttributeNS(null, 'width', HELPER_BASE_SIZE / 5 + '' );
            verticalBar.setAttributeNS(null, 'height', HELPER_BASE_SIZE / 2 + '' );

            horizontalBar.setAttributeNS(null, 'class', 'horizontal_bar');
            horizontalBar.setAttributeNS(null, 'x', HELPER_BASE_SIZE / 4 + '' );
            horizontalBar.setAttributeNS(null, 'y', (HELPER_BASE_SIZE / 5) * 2 + '' );
            horizontalBar.setAttributeNS(null, 'width', HELPER_BASE_SIZE / 2 + '' );
            horizontalBar.setAttributeNS(null, 'height', HELPER_BASE_SIZE / 5 + '' );

            connectionLine.setAttributeNS(null, 'class', 'connection_line');
            connectionLine.setAttributeNS(null, 'd',
                                          `M${HELPER_BASE_SIZE / 2},${HELPER_BASE_SIZE / 2}`
                                          + ` L${HELPER_BASE_SIZE/2},${HELPER_BASE_SIZE / 2 + HELPER_SEPARATION}`
                                         );

            inputGroup.appendChild(connectionLine);
            inputGroup.appendChild(outerCircle);
            inputGroup.appendChild(horizontalBar);
            inputGroup.appendChild(verticalBar);

            inputGroup.setAttributeNS(null, 'transform',
                                      `translate(${input_position.x - HELPER_BASE_SIZE / 2},`
                                      + ` ${input_position.y - HELPER_BASE_SIZE / 2 - HELPER_SEPARATION})`);

            const element_index = index; // Capture current index
            inputGroup.onclick = ((_ev: MouseEvent) => {
                try {
                    const transform = inputGroup.getAttributeNS(null, 'transform');

                    const position = { x: 0, y: 0 };

                    let translate = transform.match(/translate\(\s*([^\s,]+)\s*,\s*([^\s\)]+)/);
                    if (!translate) {
                        console && console.warn && console.warn('Error getting translation from', inputGroup);
                    }
                    else {
                        position.x = parseInt(translate[1]) + 15;
                        position.y = parseInt(translate[2]) - 15;
                    }

                    if (input.type === 'enum') {
                        this.createEnumValue(input, this.getBlockId(block), element_index, { position })
                    }
                    else {
                        this.createDirectValue(input.type, this.getBlockId(block), element_index, { position });
                    }
                }
                catch (err) {
                    console.error("Error creating direct value:", err);
                }
            });
        }

        this.input_helper_section.appendChild(inputHelperGroup);
        return inputHelperGroup;
    }

    private createEnumValue(input: BridgeEnumInputPortDefinition, block_id: string, input_index: number,
                            options: { position: Position2D, value?: string }) {
        const enum_input = new EnumDirectValue({
            enum_name: input.enum_name,
            enum_namespace: input.enum_namespace,
            get_values: this.getEnum,
            on_select_requested: this.onSelectRequested.bind(this),
            on_io_selected: this.onIoSelected.bind(this),
        });

        const enum_input_id = this.draw(enum_input, options.position);
        this.addConnection(new FlowConnection({ block_id: enum_input_id, output_index: 0 },
                                              { block_id: block_id, input_index: input_index },
                                             ));
    }

    private createDirectValue(type: MessageType, block_id: string, input_index: number,
                              options: { position: Position2D, value?: string }) {
        const direct_input = new DirectValue({ type: type,
                                               on_request_edit: this.onRequestEdit.bind(this),
                                               value: options.value,
                                               on_io_selected: this.onIoSelected.bind(this),
                                             });

        const direct_input_id = this.draw(direct_input, options.position);
        this.addConnection(new FlowConnection({ block_id: direct_input_id, output_index: 0 },
                                              { block_id: block_id, input_index: input_index },
                                             ));
    }

    private static MessageTypeToInputType(type: MessageType): string {
        if (!type) { type = 'any'; }

        switch(type) {
            case 'string':
            case 'any':
            case 'pulse':
                return 'text';

            case 'float':
            case 'integer':
                return 'number';

            case 'boolean':
                return 'checkbox';
        }
    }

    private editInline(block: DirectValue, type: MessageType, update: (value: string) => void): void {
        this.inlineEditor.value = block.value;
        this.inlineEditor.type = FlowWorkspace.MessageTypeToInputType(type);
        if (type === 'integer') {
            this.inlineEditor.step = '1';
        }
        else if (type === 'float') {
            this.inlineEditor.step = '0.001';
        }
        else {
            this.inlineEditor.step = '';
        }

        const valueArea = this.getWorkspaceRelArea(block.getValueArea());

        this.inlineEditorContainer.style.top = valueArea.y + 2 + 'px';
        this.inlineEditorContainer.style.left = valueArea.x + 'px';
        this.inlineEditor.style.width = valueArea.width + 'px';
        this.inlineEditor.style.height = valueArea.height - 4 + 'px';
        this.inlineEditor.style.fontSize = (1 / this.inv_zoom_level) * 100 + '%';

        this.inlineEditorContainer.classList.remove('hidden');

        const finishEdition = () => {
            this.inlineEditor.onblur = null;
            this.inlineEditor.onkeypress = null;
            this.inlineEditorContainer.classList.add('hidden');

            if (type === 'boolean') {
                update(this.inlineEditor.checked ? 'true' : 'false');
            }
            else {
                update(this.inlineEditor.value);
            }
        }

        this.inlineEditor.onblur = () => {
            finishEdition();
        };

        this.inlineEditor.onkeypress = (ev:KeyboardEvent) => {
            if (ev.key === 'Enter') {
                finishEdition();
            }
        };

        this.inlineEditor.focus();
    }

    private updateBlockInputHelpersPosition(block_id: string) {
        const block = this.blocks[block_id];

        // Deactivate helpers for all inputs in use
        let index = -1;
        for (const input of Array.from(block.input_group.children)) {
            index++;

            const input_position = this.getBlockRel(block.block, block.block.getPositionOfInput(index));
            input.setAttributeNS(null, 'transform', `translate(${input_position.x - HELPER_BASE_SIZE / 2},`
                                 + `${input_position.y - HELPER_BASE_SIZE / 2 - HELPER_SEPARATION})`);
        }
    }

    private updateBlockInputHelpersVisibility(block_id: string) {
        const block = this.blocks[block_id];

        const inputs_in_use = {};
        for (const conn_id of block.connections) {
            const conn = this.connections[conn_id];

            if (conn.connection.getSink().block_id == block_id) {
                inputs_in_use[conn.connection.getSink().input_index] = true;
            }
        }

        // Deactivate helpers for all inputs in use
        let index = -1;
        for (const input of Array.from(block.input_group.children)) {
            index++;

            if (inputs_in_use[index]) {
                input.classList.add('hidden');
            }
            else {
                input.classList.remove('hidden');
            }
        }
    }

    public removeBlock(blockId: string) {
        const info = this.blocks[blockId];
        console.log("Removing block:", info);

        // Make a copy of the array to avoid problems for modifying it during the loop
        for (const conn_id of info.connections.concat([])) {
            this.removeConnection(this.connections[conn_id].connection);
        }

        this.input_helper_section.removeChild(info.input_group);
        info.block.dispose();

        delete this.blocks[blockId];
    }

    private getBlockRel(block: FlowBlock, position: Position2D): Position2D {
        const off = block.getOffset();
        return { x: off.x + position.x, y: off.y + position.y };
    }

    private getBlockRelArea(block: FlowBlock, area: Area2D): Area2D {
        const off = block.getOffset();

        return {
            x: off.x + area.x,
            y: off.y + area.y,
            width: area.width,
            height: area.height,
        };
    }

    private getWorkspaceRelArea(area: Area2D): Area2D {
        return {
            x: (area.x - this.top_left.x) / this.inv_zoom_level,
            y: (area.y - this.top_left.y) / this.inv_zoom_level,
            width: area.width / this.inv_zoom_level,
            height: area.height / this.inv_zoom_level,
        };
    }

    private current_io_selected: {
        block: FlowBlock,
        type: 'in'|'out',
        index: number,
        definition: InputPortDefinition | OutputPortDefinition,
        port_center: Position2D,
        real_center: Position2D
    };
    private current_selecting_connector: SVGElement;

    private drawPath(path: SVGElement,
                     from: Position2D,
                     to: Position2D,
                     runway: number,
                     source_block?: FlowBlock,
                     sink_block?: FlowBlock) {
        let curve: string;

        let source_runway_direction: Direction2D = 'down';
        if (source_block) {
            source_runway_direction = source_block.getOutputRunwayDirection();
        }

        let bezier_curve = (from.y < to.y);
        if (source_block && (DirectValue === (source_block as any).__proto__.constructor)) {
            // Never use bezier curve if the target is DirectInput
            bezier_curve = true;
        }
        else if (!bezier_curve && sink_block) {
            // Another option: if a sink block was passed and the `from` point
            //  Y position is within the top and bottom of the sink, use bezier even if the position does not match.
            const area = sink_block.getBodyArea();
            bezier_curve = (from.y < (area.y + area.height / 2));
        }


        if (bezier_curve) { // Just draw a bezier curve
            const bezier_runway = runway * 2; // Compensate smoothing of the runway

            const from_runway = FlowWorkspace.addRunway(from, source_runway_direction, bezier_runway);
            const to_runway = FlowWorkspace.addRunway(to, 'up', bezier_runway);

            curve = [
                "M", from.x, ",", from.y,
                " C", from_runway.x, ",", from_runway.y,
                " ", to_runway.x, ",", to_runway.y,
                " ", to.x, ",", to.y,
            ].join("");
        }
        else { // Draw a linear circuit

            // We just try to find the X point (where the line goes "up").
            // We don't try to find the Y point and instead just use fixed "runways".
            // This makes finding the route simpler and is good enough for now.

            const from_runway = FlowWorkspace.addRunway(from, source_runway_direction, runway);
            const to_runway = FlowWorkspace.addRunway(to, 'up', runway);

            const x_cut_point = this.find_x_cut_point(from_runway, to_runway);

            curve = [
                "M", from.x, ",", from.y,
                " L", from_runway.x, ",", from_runway.y,
                " L", x_cut_point, ",", from_runway.y,
                " L", x_cut_point, ",", to_runway.y,
                " L", to_runway.x, ",", to_runway.y,
                " L", to.x, ",", to.y,
            ].join("");
        }

        path.setAttributeNS(null, "d", curve);
        path.setAttributeNS(null, 'fill', 'none');
        path.setAttributeNS(null, 'stroke', 'black');
        path.setAttributeNS(null, 'stroke-width', '1');
    }

    private static addRunway(p: Position2D, direction: Direction2D, runway: number) {
        switch (direction) {
            case 'up':
                return { x: p.x, y: p.y - runway };
            case 'down':
                return { x: p.x, y: p.y + runway };
            case 'left':
                return { x: p.x - runway, y: p.y };
            case 'right':
                return { x: p.x + runway, y: p.y };
        }
    }

    private find_x_cut_point(from: Position2D, to: Position2D): number {
        const occupied_sections: { left: number, right: number }[] = [];

        let top = from, bottom = to;
        if (from.y > to.y) {
            top = to;
            bottom = from;
        }

        for (const block_id of Object.keys(this.blocks)) {
            const block = this.blocks[block_id].block;
            if (block instanceof ContainerFlowBlock) {
                continue;
            }

            const body = block.getBodyArea();
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

    addConnection(conn: FlowConnection): boolean {
        const element = this.prepareConnection(conn);

        if (!element) {
            return false;
        }

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
            path.onmouseenter = () => {
                path.setAttributeNS(null, 'marker-end', 'url(#pulse_head_selected)');
            };
            path.onmouseleave = () => {
                path.setAttributeNS(null, 'marker-end', 'url(#pulse_head)');
            };
        }
        path.onclick = () => {
            this.removeConnection(conn);
        };
        this.connection_group.appendChild(path);

        const sink = this.blocks[conn.getSink().block_id];
        source.block.addConnection('out', conn.getSource().output_index, sink.block);
        source.connections.push(conn.id);

        sink.connections.push(conn.id);
        sink.block.addConnection('in', conn.getSink().input_index, source.block);

        this.connections[conn.id] = { connection: conn, element: path };
        this.updateBlockInputHelpersVisibility(conn.getSink().block_id);

        return path;
    }

    private removeConnection(conn: FlowConnection) {
        const source = this.blocks[conn.getSource().block_id];
        const sink = this.blocks[conn.getSink().block_id];

        // Disconnect from source
        source.block.removeConnection('out', conn.getSource().output_index, sink.block);
        const source_conn_index = source.connections.indexOf(conn.id);
        if (source_conn_index < 0) {
            console.error('Connection not found when going to remove. For block', source);
        }
        else {
            source.connections.splice(source_conn_index, 1);
        }
        this.updateBlockInputHelpersVisibility(conn.getSource().block_id);

        // Disconnect from sink
        sink.block.removeConnection('in', conn.getSink().input_index, source.block);
        const sink_conn_index = sink.connections.indexOf(conn.id);
        if (sink_conn_index < 0) {
            console.error('Connection not found when going to remove. For block', sink);
        }
        else {
            sink.connections.splice(sink_conn_index, 1);
        }
        this.updateBlockInputHelpersVisibility(conn.getSink().block_id);

        // Remove workspace information
        const info = this.connections[conn.id];
        this.connection_group.removeChild(info.element);

        delete this.connections[conn.id];
    }

    updateConnection(connection_id: string) {
        const conn = this.connections[connection_id];
        const runway = 20;

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
        this.drawPath(conn.element, source_position, sink_position, runway, source_block, sink_block);
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
        this.canvas.classList.remove('drawing');
        this.canvas.removeChild(this.current_selecting_connector);
        this.current_selecting_connector = null;
        this.current_io_selected = null;

        this.canvas.onmousemove = null;
        this.canvas.onclick = null;
    }

    // Block manager interface
    onInputsChanged(block: FlowBlock,
                    _input_num: number,
                   ): void {

        const block_id = this.getBlockId(block);
        this.drawBlockInputHelpers(block, this.blocks[block_id].input_group);
    }

    onIoSelected(block: FlowBlock,
                 type: 'in'|'out',
                 index: number,
                 definition: InputPortDefinition | OutputPortDefinition,
                 port_center: Position2D,
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
            this.canvas.classList.add('drawing');

            const runway = 20;

            this.canvas.onmousemove = ((ev: any) => {
                if (!this.canvas.contains(ev.target)) {
                    return;
                }

                const real_pointer = {
                    x: (ev.x - this.canvas.getClientRects()[0].left) * this.inv_zoom_level + this.top_left.x,
                    y: (ev.y - this.canvas.getClientRects()[0].top) * this.inv_zoom_level + this.top_left.y,
                };

                if (type == 'out') {
                    this.drawPath(this.current_selecting_connector,
                                  real_center,
                                  real_pointer,
                                  runway,
                                  block);
                }
                else {
                    this.drawPath(this.current_selecting_connector,
                                  real_pointer,
                                  real_center,
                                  runway,
                                  null,
                                  block);
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


    onSelectRequested(block: FlowBlock,
                      previous_value: string,
                      values: EnumValue[],
                      value_dict: {[key:string]: EnumValue},
                      update: (new_value: string) => void) : void {

        const backdrop = document.createElement('div');
        this.baseElement.appendChild(backdrop);
        backdrop.setAttribute('class', 'backdrop');

        const global_pos = block.getBodyElement().getClientRects()[0];
        const canvas_rect = this.canvas.getClientRects()[0];
        const abs_pos = { x: global_pos.x - canvas_rect.x, y: global_pos.y - canvas_rect.y };

        // TODO: Make this popup separate from the original block.
        // That should allow avoiding to hand-calculate coordinates, making it more responsive

        // Compensate dropdown stroke-width
        abs_pos.x -= 1;
        abs_pos.y -= 1;

        // Base positioning
        this.popupGroup.innerHTML = '';
        this.popupGroup.classList.remove('hidden');

        this.popupGroup.style.left = abs_pos.x + 'px';
        this.popupGroup.style.top = abs_pos.y + 'px';
        this.popupGroup.style.maxHeight = canvas_rect.height - abs_pos.y + 'px';

        // Editor
        const editor_container = document.createElement('div');
        const editor_input = document.createElement('input');

        editor_input.value = '';

        editor_input.style.minHeight = '3em';
        editor_container.setAttribute('class', 'editor');
        this.popupGroup.appendChild(editor_container);
        editor_container.appendChild(editor_input);

        // Option list (now empty)
        const options = document.createElement('ul');
        options.setAttribute('class', 'options');
        options.style.maxHeight = canvas_rect.height - editor_input.getClientRects()[0].height - abs_pos.y - 1 + 'px';

        this.popupGroup.appendChild(options);

        // Set callbacks functions
        const close = () => {
            this.baseElement.removeChild(backdrop);
            this.popupGroup.innerHTML = '';
            this.popupGroup.classList.add('hidden');
        };
        const select_value = (val: string) => {
            close();
            if (this.variables_in_use[previous_value]) {
                this.variables_in_use[previous_value]--;
            }
            if (!this.variables_in_use[val]) {
                this.variables_in_use[val] = 0;
            }
            this.variables_in_use[val]++;

            update(val);
        };


        const MAX_RESULTS = 100; // Max. results to show at a single time
        const TIME_WAIT_FOR_SEARCH_TIME = 200; // Time to wait for next input before attempting to search
        const SCROLL_OPTIONS: ScrollIntoViewOptions = {
            behavior: 'smooth',
            block: 'nearest',
        };

        let bounce_control = null;
        let last_query = null;

        let selected_index = null;

        // Keyup if controled instead of keypress
        // as ArrowRight and ArrowLeft are not triggered on keypress
        editor_input.onkeyup = (ev: KeyboardEvent) => {
            if (ev.key === 'ArrowUp' || ev.key === 'ArrowDown'
                || ev.key === 'PageDown' || ev.key === 'PageUp'
                || ev.key === 'Home' || ev.key === 'End'
               ) {
                const old_index = selected_index;
                let scroll_options: ScrollIntoViewOptions = { behavior: SCROLL_OPTIONS.behavior, block: SCROLL_OPTIONS.block };

                if (ev.key === 'ArrowUp') {
                    if (selected_index) {
                        selected_index--;
                    }
                    else {
                        selected_index = 0;
                    }
                }
                else if (ev.key === 'ArrowDown') {
                    if (selected_index === null) {
                        selected_index = 0;
                    }
                    else {
                        selected_index++;
                    }
                }
                else if (ev.key === 'PageDown') {
                    if (selected_index === null) {
                        selected_index = 0;
                    }
                    else {
                        scroll_options.block = 'start';
                        const children = options.children;
                        const parent = options.getClientRects()[0];
                        // Go down the list until an element is not in view
                        // this is not done directly with `inc = size(container) / size(next_element)`
                        // to support for cases where elements have different sizes
                        for (; selected_index < children.length; selected_index++) {
                            const child = children[selected_index].getClientRects()[0];
                            if (child.bottom > parent.bottom) {
                                break;
                            }
                        }
                    }
                }
                else if (ev.key === 'PageUp') {
                    if (!selected_index) {
                        selected_index = 0;
                    }
                    else {
                        selected_index--;
                        scroll_options.block = 'end';
                        const children = options.children;
                        const parent = options.getClientRects()[0];
                        // Go down the list until an element is not in view
                        // this is not done directly with `inc = size(container) / size(next_element)`
                        // to support for cases where elements have different sizes
                        for (; selected_index > 0; selected_index--) {
                            const child = children[selected_index].getClientRects()[0];
                            if (child.top < parent.top) {
                                break;
                            }
                        }
                    }
                }
                else if (ev.key === 'Home') {
                    selected_index = 0;
                }
                else if (ev.key === 'End') {
                    selected_index = options.children.length - 1;
                }

                try {
                    if (old_index !== null) {
                        options.children[old_index].classList.remove('selected');
                    }

                    const selected = options.children[selected_index];
                    selected.classList.add('selected');
                    selected.scrollIntoView(scroll_options);
                }
                catch(err) {
                    console.warn(err);
                }
            }
            else if (ev.key === 'Enter') {
                if (selected_index !== null) {
                    (options.children[selected_index] as HTMLElement).click();
                }
            }

            // Avoid updating when no change has been made
            if (last_query !== editor_input.value) {
                if (bounce_control) {
                    clearTimeout(bounce_control);
                }
                bounce_control = setTimeout(() => {
                    bounce_control = null;
                    update_values();
                }, TIME_WAIT_FOR_SEARCH_TIME);
            }
        }

        backdrop.onclick = () => {
            close();
        };

        const update_values = () => {
            selected_index = null;
            const query = editor_input.value;
            last_query = query;

            let matches = values;
            if (query) {
                const options = {
                    isCaseSensitive: false,
                    findAllMatches: false,
                    includeMatches: false,
                    includeScore: true,
                    useExtendedSearch: false,
                    minMatchCharLength: 1,
                    shouldSort: true,
                    threshold: 0.6,
                    location: 0,
                    distance: 10,
                    keys: [
                        "name",
                    ]
                };

                const fuse = new Fuse(values, options);

                // Change the pattern
                const results = fuse.search(query);
                results.sort((x: any, y: any) => (x as any).score - (y as any).score );
                if (!matches) {
                    return; // Don't update
                }

                matches = results.map((v: { item: any; }) => v.item);
            }

            // Options
            options.innerHTML = ''; // Clear children
            for (const value of matches.slice(0, MAX_RESULTS)) {
                const e = document.createElement('li');
                e.innerText = value.name;
                options.appendChild(e);
                e.onclick = () => {
                    select_value(value.id);
                }
            }
            // Scroll options up
            options.children[0].scrollIntoView(SCROLL_OPTIONS);
        };

        update_values();
        editor_input.focus();
    }

    onDropdownExtended(block: FlowBlock,
                       slot_id: string,
                       previous_value: string,
                       current_rect: Area2D,
                       update: (new_value: string) => void,
                      ): void {

        const backdrop = document.createElement('div');
        this.baseElement.appendChild(backdrop);
        backdrop.setAttribute('class', 'backdrop');

        const edition_area = this.getBlockRelArea(block, current_rect);
        const abs_area = this.getWorkspaceRelArea(edition_area);

        // Compensate dropdown stroke-width
        abs_area.x -= 1;
        abs_area.y -= 1;
        abs_area.width += 2;
        abs_area.height += 2;

        // Base positioning
        this.popupGroup.innerHTML = '';

        this.popupGroup.style.left = abs_area.x + 'px';
        this.popupGroup.style.top = abs_area.y + 'px';

        // Editor
        const editor_container = document.createElement('div');
        const editor_input = document.createElement('input');

        editor_input.value = previous_value;
        editor_input.style.minWidth = abs_area.width + 'px';
        editor_input.style.minHeight = abs_area.height + 'px';
        editor_container.setAttribute('class', 'editor');
        this.popupGroup.appendChild(editor_container);
        editor_container.appendChild(editor_input);

        // Set callbacks functions
        const close = () => {
            this.baseElement.removeChild(backdrop);
            this.popupGroup.innerHTML = '';
            this.popupGroup.classList.add('hidden');
        };
        const select_value = (val: string) => {
            close();
            if (this.variables_in_use[previous_value]) {
                this.variables_in_use[previous_value]--;
            }
            if (!this.variables_in_use[val]) {
                this.variables_in_use[val] = 0;
            }
            this.variables_in_use[val]++;

            update(val);
        };

        editor_input.onkeypress = (ev:KeyboardEvent) => {
            if (ev.key === 'Enter') {
                select_value(editor_input.value);
            }
        };

        backdrop.onclick = () => {
            close();
        };

        // Options
        const options = document.createElement('ul');
        options.setAttribute('class', 'options');
        this.popupGroup.appendChild(options);

        let option_list = [];
        if (slot_id === 'variable') {
            for (const var_name of Object.keys(this.variables_in_use)) {
                if (this.variables_in_use[var_name] > 0) {
                    option_list.push(var_name);
                }
            }
        }

        if (option_list.length === 0) {
            option_list.push('i');
        }

        for (const option of option_list) {
            const e = document.createElement('li');
            e.innerText = option;
            options.appendChild(e);
            e.onclick = () => {
                select_value(option);
            }
        }

        this.popupGroup.classList.remove('hidden');
    }

    onRequestEdit(block: DirectValue, type: MessageType, update: (value: string) => void): void {
        this.editInline(block, type, update);
    }
    // </Block manager interface>
}
