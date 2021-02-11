import { AtomicFlowBlock, AtomicFlowBlockData } from './atomic_flow_block';
import { BlockManager } from './block_manager';
import { DirectValue } from './direct_value';
import { EnumDirectValue, EnumGetter, EnumValue } from './enum_direct_value';
import { Area2D, BridgeEnumInputPortDefinition, ContainerBlock, Direction2D, FlowBlock, FlowBlockData, InputPortDefinition, MessageType, OutputPortDefinition, Position2D, Resizeable } from './flow_block';
import { FlowConnectionData, SourceDefinition, SinkDefinition, setConnectionType } from './flow_connection';
import { FlowGraph, FlowGraphEdge, FlowGraphNode } from './flow_graph';
import { Toolbox } from './toolbox';
import { ContainerFlowBlock, ContainerFlowBlockData, isContainerFlowBlockData } from './ui-blocks/container_flow_block';
import { UiFlowBlock, UiFlowBlockData } from './ui-blocks/ui_flow_block';
import { isContainedIn, uuidv4, maxKey } from './utils';
import { MatDialog } from '@angular/material/dialog';
import { ConfigureBlockDialogComponent, ConfigurableBlock, BlockConfigurationOptions } from './dialogs/configure-block-dialog/configure-block-dialog.component';
import { ProgramService } from '../program.service';
import { CannotSetAsContentsError } from './ui-blocks/cannot_set_as_contents_error';
import * as Y from 'yjs';
import { WebsocketProvider } from 'y-websocket'
import { ProgramEditorEventValue } from 'app/program';
import { Synchronizer } from 'app/syncronizer';
import { EnvironmentService } from 'app/environment.service';

/// <reference path="../../../node_modules/fuse.js/dist/fuse.d.ts" />
declare const Fuse: any;

const SvgNS = "http://www.w3.org/2000/svg";

const ABSOLUTE_MAX_ITERATIONS = 100;
const DEFAULT_MAX_ITERATIONS = 10;

const INV_MAX_ZOOM_LEVEL = 5;
const TIME_BETWEEN_POSITION_ITERATIONS = 100; // In milliseconds

const CUT_POINT_SEARCH_INCREASES = 10;
const CUT_POINT_SEARCH_SPACING = CUT_POINT_SEARCH_INCREASES;

// Draw helper
const HELPER_BASE_SIZE = 25;
const HELPER_SEPARATION = HELPER_BASE_SIZE * 1.5;

// Zoom management
const SMALL_ZOOM_INCREMENTS = 0.1;
const LARGE_ZOOM_INCREMENTS = 0.25;
const FAB_BUTTON_PADDING = 5;

type ConnectableNode = {
    block: FlowBlock,
    type: 'in' | 'out',
    index: number,
};

type State = 'waiting'     // Base state
    | 'dragging-block'     // Moving around a block
    | 'dragging-workspace' // Moving around the workspace
    | 'selecting-workspace'
    ;

type SharedBlockData = {
    connections: string[];
    container_id: string | null;
    position: Position2D;
    blockData: FlowBlockData,
};

type NonReadyReason = 'loading' | 'disconnected';

export class FlowWorkspace implements BlockManager {
    private eventStream: Synchronizer<ProgramEditorEventValue>;
    private isReady: boolean;
    private nonReadyReason: NonReadyReason | null;
    private eventSubscription: any;
    private cursorDiv: HTMLElement;
    private cursorInfo: {[key: string]: HTMLElement};
    private wsSyncProvider: WebsocketProvider;


    public static BuildOn(baseElement: HTMLElement,
                          getEnum: EnumGetter,
                          dialog: MatDialog,
                          programId: string,
                          programService: ProgramService,
                          read_only: boolean,
                          environmentService: EnvironmentService,
                         ): FlowWorkspace {
        let workspace: FlowWorkspace;
        try {
            workspace = new FlowWorkspace(baseElement, getEnum, dialog, programId, programService, read_only, environmentService);
            workspace.init();
        }
        catch(err) {
            console.error(err);
            workspace.dispose();

            throw err;
        }

        return workspace;
    }

    public setToolbox(toolbox: Toolbox) {
        this.toolbox = toolbox;
    }

    public onResize() {
        this.update_top_left();
    }

    public getCanvas(): SVGSVGElement {
        return this.canvas;
    }

    public getGraph(): FlowGraph {
        const blocks: { [key: string]: FlowGraphNode } = {};
        for (const block_id of Object.keys(this.blockObjs)) {
            const blockObj = this.blockObjs[block_id].block;
            const serialized = blockObj.serialize();
            const position = blockObj.getOffset();

            blocks[block_id] = { data: serialized, position: position, container_id: this.blocks.get(block_id)?.container_id };
        }

        const connections: FlowGraphEdge[] = [];

        for (const conn_id of Array.from(this.connections.keys())) {
            const connection = this.connections.get(conn_id);

            const source = connection.source;
            const sink = connection.sink;
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

    public getPages(): {[key: string]: any} {
        const pages: { [key: string]: any } = {};
        for (const block_id of Object.keys(this.blockObjs)) {
            const block = this.blockObjs[block_id].block;
            if (block instanceof ContainerFlowBlock) {
                if (block.isPage) {
                    pages['/'] = { value: block.renderAsUiElement(), title: block.getPageTitle() };
                }
            }
        }

        return pages;
    }

    public load(graph: FlowGraph) {
        this.autoposition = false;

        // TODO: Merge with _sortByDependencies?
        let to_go = Object.keys(graph.nodes);

        let processing = true;

        while ((to_go.length > 0) && processing) {
            processing = false;
            const skipped = [];

            for (const block_id of to_go) {
                const block = graph.nodes[block_id];
                if (block.container_id && (!this.blockObjs[block.container_id])) {
                    skipped.push(block_id);
                    continue;
                }

                const created_block = this.deserializeBlock(block_id, block.data);

                if (!created_block) {
                    console.error("Error deserializing block:", block.data);
                    continue;
                }

                try {
                    this.draw(created_block, block.position);
                }
                catch (err) {
                    console.error("Error drawing block", err);
                    continue;
                }

                if (block.container_id) {
                    try {
                        this._updateBlockContainer(created_block, this.blockObjs[block.container_id].block);
                    }
                    catch (err) {
                        if (err instanceof CannotSetAsContentsError) {
                            this._updateBlockContainer(created_block, null);
                        }
                        else {
                            throw err;
                        }
                    }
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
                        block: this.blockObjs[conn.from.id].block,
                        type: 'out',
                        index: conn.from.output_index,
                    },
                    {
                        block: this.blockObjs[conn.to.id].block,
                        type: 'in',
                        index: conn.to.input_index,
                    },
                )
            }
            catch(err) {
                console.error("Error establishing connection", err);
            }
        }

        this._initializeReady();
    }

    public initializeEmpty() {
        this._initializeReady();
    }

    private _initializeReady() {
        this.autoposition = true;

        this._initializeEventSynchronization();
    }

    private _initializeEventSynchronization() {
        // Initialize editor event listeners
        // This is used for collaborative editing.

        if (this.read_only || !this.environmentService.hasYjsWsSyncServer()) {
            this.becomeReady(); // We won't have to wait for the last state to get loaded
            return;
        }

        this.blocks.observe(this._onBlockChange.bind(this));
        this.connections.observe(this._onConnectionChange.bind(this));

        // HACK: Give some space to blocks and connections to sync before establishing connection
        setTimeout(() => {
            const [base, opts] = this.programService.getProgramStreamingEventsUrlParts(this.programId);
            const room = 'yjs';

            this.wsSyncProvider = new WebsocketProvider(this.environmentService.getYjsWsSyncServer(),
                                                        this.programId,
                                                        this.doc,
                                                        { params: opts });

            this.eventStream = this.programService.getEventStream(this.programId, { skip_previous: true });
            this.eventSubscription = this.eventStream.subscribe(
                {
                    next: (ev: ProgramEditorEventValue) => {
                        if (ev.type === 'cursor_event') {
                            this.drawPointer(ev.value);
                        }
                        else if (ev.type === 'add_editor') {
                            this.newPointer(ev.value.id);
                        }
                        else if (ev.type === 'remove_editor') {
                            this.deletePointer(ev.value.id);
                        }
                        else if (ev.type === 'ready') {
                            this.becomeReady();
                        }
                    },
                    error: (error: any) => {
                        console.error("Error obtainig editor events:", error);
                    },
                    complete: () => {
                        console.log("Disconnected");
                        this.nonReadyReason = 'disconnected';
                        this.isReady = false;
                    }
                }
            );

            const onMouseEvent = ((ev: MouseEvent) => {
                const disp = this.getEditorPosition();

                const rect = this.baseElement.getBoundingClientRect();
                const cursorInWorkspace = { x: ev.x - rect.left, y: ev.y - rect.top }

                const posInCanvas = {
                    x: cursorInWorkspace.x / disp.scale - disp.x,
                    y: cursorInWorkspace.y / disp.scale - disp.y,
                }

                this.eventStream.push({ type: 'cursor_event', value: posInCanvas })
            });

            this.baseElement.onmousemove = onMouseEvent;
            this.baseElement.onmouseup = onMouseEvent;
        }, 0);
    }

    /* Collaborator pointer management */
    newPointer(id: string): HTMLElement {
        const cursor = document.createElement('object');
        cursor.type = 'image/svg+xml';
        cursor.style.display = 'none';
        cursor.style.position = 'fixed';
        cursor.style.height = '2.5ex';
        cursor.style.color
        cursor.style.zIndex = '10';
        cursor.style.pointerEvents = 'none';
        cursor.data = '/assets/cursor.svg';
        cursor.onload = () => {
            // Give the cursor a random color
            cursor.getSVGDocument().getElementById('cursor').style.fill = `hsl(${Math.random() * 255},50%,50%)`;
        };

        this.cursorDiv.appendChild(cursor);
        this.cursorInfo[id] = cursor;

        return cursor;
    }

    getPointer(id: string): HTMLElement {
        if (this.cursorInfo[id]) {
            return this.cursorInfo[id];
        }

        return this.newPointer(id);
    }

    deletePointer(id: string) {
        const cursor = this.cursorInfo[id];
        if (!cursor) {
            return;
        }
        this.cursorDiv.removeChild(cursor);
        delete this.cursorInfo[id];
    }

    drawPointer(pos:{id: string, x : number, y: number}) {
        const rect = this.baseElement.getBoundingClientRect();
        const disp = this.getEditorPosition();
        const cursor = this.getPointer(pos.id);

        const posInScreen = {
            x: (pos.x + disp.x) * disp.scale + rect.left,
            y: (pos.y + disp.y) * disp.scale + rect.top,
        }
        cursor.style.left = posInScreen.x + 'px';
        cursor.style.top = posInScreen.y + 'px';

        let inEditor = false;
        if (rect.left <= posInScreen.x && rect.right >= posInScreen.x) {
            if (rect.top <= posInScreen.y && rect.bottom >= posInScreen.y) {
                inEditor = true;
            }
        }

        if (inEditor) {
            cursor.style.display = 'block';
        }
        else {
            cursor.style.display = 'none';
        }
    }

    becomeReady() {
        this.isReady = true;
    }


    getEditorPosition(): {x:number, y: number, scale: number} | null {
        return {
            x: -this.top_left.x,
            y: -this.top_left.y,
            scale: 1/this.inv_zoom_level,
        }
    }


    private deserializeBlock(blockId: string, blockData: FlowBlockData) {
        switch (blockData.type) {
            case AtomicFlowBlock.GetBlockType():
                return AtomicFlowBlock.Deserialize(blockData as AtomicFlowBlockData, blockId, this);

            case UiFlowBlock.GetBlockType():
                if (isContainerFlowBlockData(blockData)) {
                    return ContainerFlowBlock.Deserialize(blockData as ContainerFlowBlockData, blockId, this, this.toolbox);
                }
                else {
                    return UiFlowBlock.Deserialize(blockData as UiFlowBlockData, blockId, this, this.toolbox);
                }

            case DirectValue.GetBlockType():
                return DirectValue.Deserialize(blockData, blockId, this);

            case EnumDirectValue.GetBlockType():
                return EnumDirectValue.Deserialize(blockData, blockId, this, this.getEnum);

            default:
                console.error("Unknown block type:", blockData.type);
        }
    }

    private numPages = 0;
    private baseElement: HTMLElement;
    private inlineEditorContainer: HTMLDivElement;
    private inlineEditor: HTMLInputElement;
    private inlineMultilineEditor: HTMLTextAreaElement;
    private state: State = 'waiting';
    private toolbox: Toolbox;
    private autoposition: boolean;

    private popupGroup: HTMLDivElement;
    private canvas: SVGSVGElement;
    private selectionRect: SVGRectElement;

    private connection_group: SVGGElement;
    private block_group: SVGGElement;
    private container_group: SVGGElement;
    private containers: (FlowBlock & ContainerBlock)[] = [];

    private top_left = { x: 0, y: 0 };
    private inv_zoom_level = 1;
    private input_helper_section: SVGGElement;
    private trashcan: SVGGElement;
    private button_group: SVGGElement;
    private variables_in_use: { [key: string]: number } = {};
    private getEnum: EnumGetter;

    public getInvZoomLevel(): number {
        return this.inv_zoom_level;
    }

    private doc: Y.Doc;
    private blocks: Y.Map<SharedBlockData>;
    private connections: Y.Map<FlowConnectionData>;

    private connectionElements: {[key: string]: SVGElement} = {};
    private blockObjs: {[key: string]: {
        block: FlowBlock,
        input_group: SVGGElement,
    }} = {};

    private _selectedBlocks: string[] = [];


    public getDialog(): MatDialog {
        return this.dialog;
    }

    private constructor(baseElement: HTMLElement,
                        getEnum: EnumGetter,
                        private dialog: MatDialog,
                        private programId: string,
                        private programService: ProgramService,
                        private read_only: boolean,
                        private environmentService: EnvironmentService,
                       ) {
        this.baseElement = baseElement;
        this.getEnum = getEnum;

        this.doc = new Y.Doc();

        this.blocks = this.doc.getMap('blocks');
        this.connections = this.doc.getMap('connections');
    }

    private _onBlockChange(event: Y.YMapEvent<SharedBlockData>, _transaction: Y.Transaction) {
        const moves = [] as string[];
        event.changes.keys.forEach((change, key) => {
            if (change.action === 'add') {
                console.log(`BLOCK "${key}" was added. Initial value:`, this.blocks.get(key));

                if (key in this.blockObjs) {
                    console.log("Already contained");
                    return;
                }

                const block = this.deserializeBlock(key, this.blocks.get(key).blockData);
                this.draw(block, this.blocks.get(key).position);

                moves.push(key);

                if (block instanceof UiFlowBlock) {
                    this._updateBlockContainerFromContainer(block,
                                                            this.blockObjs[this.blocks.get(key).container_id]?.block,
                                                            null);
                }
            }
            else if (change.action === 'update') {
                console.log(`BLOCK "${key}" was updated.`);

                // Note that moveTo() does not trigger `block.onMove()` callbacks.
                const block = this.blockObjs[key].block;
                const newData = this.blocks.get(key);
                block.moveTo(newData.position);
                this._afterBlocksMove([key]);

                if (block instanceof UiFlowBlock) {
                    if (newData.container_id !== change.oldValue.container_id) {
                        this._updateBlockContainerFromContainer(block,
                                                                this.blockObjs[newData.container_id]?.block,
                                                                this.blockObjs[change.oldValue.container_id]?.block);
                    }
                }

                // Updated block data
                const updatedOptions = JSON.stringify(newData.blockData) === JSON.stringify(change.oldValue.blockData);
                block.updateOptions(newData.blockData);
            }
            else if (change.action === 'delete') {
                console.log(`Property "${key}" was deleted. New value: undefined. Previous value: "${change.oldValue}".`)

                if (!(key in this.blockObjs)) {
                    console.log("Already deleted");
                    return;
                }

                const block = this.blockObjs[key].block;

                if (block instanceof UiFlowBlock) {
                    this._updateBlockContainerFromContainer(block, null, this.blockObjs[change.oldValue.container_id]?.block);
                }

                this.removeBlock(key, change.oldValue);

            }
        });
        this._afterBlocksMove(moves)
    }

    private _onConnectionChange(event: Y.YMapEvent<FlowConnectionData>, _transaction: Y.Transaction) {
        event.changes.keys.forEach((change, key) => {
            if (change.action === 'add') {
                const conn: FlowConnectionData = this.connections.get(key);
                this.addConnection(conn.source, conn.sink);
            } else if (change.action === 'update') {
                console.log(`[Conn] Property "${key}" was updated. New value: "${this.connections.get(key)}". Previous value:`, change.oldValue);
                // TODO: In which case can this happen?
            } else if (change.action === 'delete') {
                console.log(`RM ${key} - ${change.oldValue.id}`);
                if (key in this.connectionElements) {
                    this.removeConnection(change.oldValue);
                }
                else {
                    console.debug(`Attempting to remove inexisting connection: ${key}.`);
                }
            }
        })
    }

    public onBlockOptionsChanged(block: FlowBlock) {
        console.log("==>", block);
        console.log("OPTIONS", block.serialize());

        const serialized = block.serialize();
        const saveData = this.blocks.get(block.id);
        saveData.blockData = serialized;
        this.blocks.set(block.id, saveData);
    }

    private init() {
        // Inline editor
        this.inlineEditorContainer = document.createElement('div');
        this.inlineEditorContainer.setAttribute('class', 'inline_editor_container hidden');
        this.baseElement.appendChild(this.inlineEditorContainer);
        this.inlineEditor = document.createElement('input');
        this.inlineEditorContainer.appendChild(this.inlineEditor);
        this.inlineMultilineEditor = document.createElement('textarea');
        this.inlineEditorContainer.appendChild(this.inlineMultilineEditor);

        // Popup group
        this.popupGroup = document.createElement('div');
        this.popupGroup.setAttribute('class', 'popup_group hidden');
        this.baseElement.appendChild(this.popupGroup);

        this.canvas = document.createElementNS(SvgNS, "svg");
        this.canvas.setAttribute('class', 'block_renderer ' + (this.read_only ? "read-only" : ''));

        this.selectionRect = document.createElementNS(SvgNS, "rect");
        this.selectionRect.setAttribute('class', 'selection');

        this.input_helper_section = document.createElementNS(SvgNS, "g");
        this.trashcan = document.createElementNS(SvgNS, "g");
        this.button_group = document.createElementNS(SvgNS, "g");

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
        this.canvas.appendChild(this.button_group);
        this.canvas.appendChild(this.selectionRect);

        this.baseElement.appendChild(this.canvas);

        this.init_definitions();
        this.set_events();
        this.init_trashcan();
        this.init_buttons();
        this.init_cursors();

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
<linearGradient id="user-pulse-pattern" x1="0" x2="30%" gradientTransform="rotate(45)" spreadMethod="repeat">
  <stop offset="0%"   stop-color="var(--user-pulse-1)" />
  <stop offset="49%"  stop-color="var(--user-pulse-1)" />
  <stop offset="50%"  stop-color="var(--user-pulse-2)" />
  <stop offset="100%" stop-color="var(--user-pulse-2)" />
</linearGradient>
<linearGradient id="user-pulse-wire-pattern" x1="0" x2="30%" gradientTransform="rotate(90)" spreadMethod="repeat">
  <stop offset="0%"   stop-color="var(--user-pulse-1)" />
  <stop offset="49%"  stop-color="var(--user-pulse-1)" />
  <stop offset="50%"  stop-color="var(--user-pulse-2)" />
  <stop offset="100%" stop-color="var(--user-pulse-2)" />
</linearGradient>
`;

        this.connection_group.appendChild(definitions);
    }

    private init_trashcan() {
        this.trashcan.setAttribute('class', 'trashcan helper ' + (this.read_only ? 'invisible' : '') );

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

    private init_buttons() {
        this.button_group.setAttribute('class', 'fab-button-group');
        let button_size = null;

        {
            const zoom_in_button = document.createElementNS(SvgNS, 'g');
            zoom_in_button.setAttribute('class', 'button');
            zoom_in_button.onclick = () => { this.zoom_in(); }
            const shadow = document.createElementNS(SvgNS, 'circle');
            shadow.setAttributeNS(null, 'class', 'button-shadow');
            shadow.setAttributeNS(null, 'r', '2ex');

            const body = document.createElementNS(SvgNS, 'circle');
            body.setAttributeNS(null, 'class', 'button-body');
            body.setAttributeNS(null, 'r', '2ex');

            const symbol = document.createElementNS(SvgNS, 'path');
            symbol.setAttributeNS(null, 'class', 'button-symbol');
            symbol.setAttributeNS(null, 'd', 'M-10,0 h20 m-10,-10 v20'); // A `+` sign

            zoom_in_button.appendChild(shadow);
            zoom_in_button.appendChild(body);
            zoom_in_button.appendChild(symbol);
            this.button_group.appendChild(zoom_in_button);

            button_size = zoom_in_button.getBBox();
        }

        {
            const zoom_out_button = document.createElementNS(SvgNS, 'g');
            zoom_out_button.setAttribute('class', 'button');
            zoom_out_button.setAttribute('transform', `translate(0, ${ button_size.height + FAB_BUTTON_PADDING * 2 })`);
            zoom_out_button.onclick = () => { this.zoom_out(); }
            const shadow = document.createElementNS(SvgNS, 'circle');
            shadow.setAttributeNS(null, 'class', 'button-shadow');
            shadow.setAttributeNS(null, 'r', '2ex');

            const body = document.createElementNS(SvgNS, 'circle');
            body.setAttributeNS(null, 'class', 'button-body');
            body.setAttributeNS(null, 'r', '2ex');

            const symbol = document.createElementNS(SvgNS, 'path');
            symbol.setAttributeNS(null, 'class', 'button-symbol');
            symbol.setAttributeNS(null, 'd', 'M-10,0 h20'); // A `-` sign

            zoom_out_button.appendChild(shadow);
            zoom_out_button.appendChild(body);
            zoom_out_button.appendChild(symbol);
            this.button_group.appendChild(zoom_out_button);
        }

        {
            const zoom_reset_button = document.createElementNS(SvgNS, 'g');
            zoom_reset_button.setAttribute('class', 'button');
            zoom_reset_button.setAttribute('transform', `translate(0, ${ (button_size.height + FAB_BUTTON_PADDING * 2) * 2 })`);
            zoom_reset_button.onclick = () => { this.zoom_reset(); }
            const shadow = document.createElementNS(SvgNS, 'circle');
            shadow.setAttributeNS(null, 'class', 'button-shadow');
            shadow.setAttributeNS(null, 'r', '2ex');

            const body = document.createElementNS(SvgNS, 'circle');
            body.setAttributeNS(null, 'class', 'button-body');
            body.setAttributeNS(null, 'r', '2ex');

            const symbol = document.createElementNS(SvgNS, 'path');
            symbol.setAttributeNS(null, 'class', 'button-symbol');
            symbol.setAttributeNS(null, 'd', 'M-10,-5 h20 m-20,10 h20'); // An `=` sign

            zoom_reset_button.appendChild(shadow);
            zoom_reset_button.appendChild(body);
            zoom_reset_button.appendChild(symbol);
            this.button_group.appendChild(zoom_reset_button);
        }
    }

    private init_cursors() {
        this.cursorDiv = document.getElementById('program-cursors');
        this.cursorInfo = {};
    }

    private set_events() {
        let lastMouseDownTime: null | Date = null;
        const startMove: (ev?: MouseEvent) => (() => void) = ((ev: MouseEvent | undefined)  => {
            let last = ev ?  { x: ev.x, y: ev.y } : null;

            this.state = 'dragging-workspace';
            this.canvas.classList.add('dragging');

            this.canvas.onmousemove = ((ev: MouseEvent) => {
                if (last) {
                    this.top_left.x -= (ev.x - last.x) * this.inv_zoom_level;
                    this.top_left.y -= (ev.y - last.y) * this.inv_zoom_level;
                }
                last = { x: ev.x, y: ev.y };

                this.update_top_left();
            });

            return () => {
                this.state = 'waiting';
                this.canvas.classList.remove('dragging');
                this.canvas.onmousemove = null;
            }
        });

        this.canvas.onmousedown = (ev: MouseEvent) => {
            if (this.state !== 'waiting') {
                return;
            }

            this.ensureContextMenuHidden();

            const time = new Date();
            if (!this.read_only && lastMouseDownTime && (((time as any) - (lastMouseDownTime as any)) < 1000))  {
                const start = this._getPositionFromEvent(ev);
                this.state = 'selecting-workspace';
                this.canvas.classList.add('selecting');
                this._updateSelectionRectangle(start, start);

                this.canvas.onmousemove = ((ev: MouseEvent) => {
                    this._updateSelectionRectangle(start, this._getPositionFromEvent(ev));
                });

                this.canvas.onmouseup = (() => {
                    // TODO: Do something with the selection
                    this.state = 'waiting';
                    this.canvas.classList.remove('selecting');
                    this.canvas.onmousemove = null;
                    this.canvas.onmouseup = null;
                });
            }
            else {
                lastMouseDownTime = time;

                const stopMove = startMove(ev);
                this.canvas.onmouseup = (() => {
                    this.canvas.onmouseup = null;
                    stopMove();
                });
            }
        };

        // Capture key presses directed to canvas
        document.body.onkeydown = ((ev: KeyboardEvent) => {
            if (this.state !== 'waiting') {
                return;
            }

            if ((ev.target === document.body) && (ev.code === 'Space')) {
                const stopMove = startMove(null);

                document.body.onkeyup = ((ev: KeyboardEvent) => {
                    if (ev.code === 'Space') {
                        document.body.onkeyup = null;
                        stopMove();
                    }
                });

            }
        });

        this.canvas.ontouchstart = ((ev: TouchEvent) => {
            if (this.state !== 'waiting') {
                return;
            }

            if (ev.target !== this.canvas) {
                return;
            }

            lastMouseDownTime = new Date();
            // TODO: Implement select mode

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
                    console.error("Unexpected action with more than one touch", ev);
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
        });
    }

    private _updateSelectionRectangle(origin: Position2D, edge: Position2D) {
        const topLeft = { x: Math.min(origin.x, edge.x), y: Math.min(origin.y, edge.y) };
        const botRight = { x: Math.max(origin.x, edge.x), y: Math.max(origin.y, edge.y) };
        const area = this.absPosToWorkspace({
            x: topLeft.x,
            y: topLeft.y,
            width: botRight.x - topLeft.x,
            height: botRight.y - topLeft.y
        });

        this.selectionRect.setAttributeNS(null, 'x', area.x + '');
        this.selectionRect.setAttributeNS(null, 'y', area.y + '');
        this.selectionRect.setAttributeNS(null, 'width', area.width + '');
        this.selectionRect.setAttributeNS(null, 'height', area.height + '');

        const blocks = this._getBlocksInArea(area);

        // Discard blocks that cannot be selected
        const selectableBlocks = blocks.filter(b => {
            const block = this.blockObjs[b].block;

            return !((block instanceof ContainerFlowBlock) && (block.isPage));
        });
        this.updateSelectBlockList(selectableBlocks);
    }

    private _getBlocksInArea(area: Area2D): string[] {
        const blocks = [];

        for (const blockId of Object.keys(this.blockObjs)) {
            const blockArea = this.blockObjs[blockId].block.getBodyArea();
            if (isContainedIn(blockArea, area)) {
                blocks.push(blockId);
            }
        }

        return blocks;
    }

    private updateSelectBlockList(blockIds: string[]) {
        // Find blocks that are added and removed from the selection
        const added = [];
        const removed = [];
        for (const blockId of blockIds) {
            if (this._selectedBlocks.indexOf(blockId) < 0) {
                added.push(blockId);
            }
        }

        for (const blockId of this._selectedBlocks) {
            if (blockIds.indexOf(blockId) < 0) {
                removed.push(blockId);
            }
        }

        // Update blocks style
        added.forEach(blockId => {
            const block = this.blockObjs[blockId].block;
            block.getBodyElement().classList.add('selected');
            block.onGetFocus();
        })
        removed.forEach(blockId => {
            const blockObj = this.blockObjs[blockId];
            if (blockObj) {
                blockObj.block.onLoseFocus();
                blockObj.block.getBodyElement().classList.remove('selected');
            }
            else {
                console.error(`Error unselecting block (id: ${blockId}). Block not found.`);
            }
        })

        this._selectedBlocks = blockIds.concat([]); // Clone the list, just for safety
        this._raiseSelectedBlocks();
    }

    private _raiseSelectedBlocks() {
        this._raiseBlocks(this._selectedBlocks);
    }

    private _raiseBlocks(blockIds: string[]) {
        const allBlocksUnder = this._getAllBlocksContainedInGroup(blockIds);
        const sortedBlocks = this._sortByDependencies(allBlocksUnder);

        for (const id of sortedBlocks) {
            const block = this.blockObjs[id].block;
            const element = block.getBodyElement();

            element.parentNode.appendChild(element);
        }
    }

    private _getAllBlocksContainedInGroup(blockIds: string[]): string[] {
        // From a list of blocks, add to it all the blocks contained in its
        // Container blocks.

        const allKnown: {[key: string]: boolean} = {}; // Avoid duplicated results
        for (const id of blockIds) {
            if (allKnown[id]) {
                // Already explored branch
                continue;
            }

            allKnown[id] = true;
            const blockObj = this.blockObjs[id];

            if (blockObj.block instanceof ContainerFlowBlock) {

                for (const content of blockObj.block.recursiveGetAllContents()) {
                    const contentId = content.id;
                    allKnown[contentId] = true;
                }
            }
        }

        return Object.keys(allKnown);
    }

    private _sortByDependencies(blockIds: string[]): string[] {
        let to_go = blockIds.concat([]).sort(); // First sort alphabetically, to stabilize the result
        const sortedByDep = [];

        const processedById: {[key: string]: boolean } = {};
        for (const blockId of blockIds) {
            // This is used to differenciate between dependencies not yet
            // processed or not to be sorted.
            processedById[blockId] = false;
        }

        let processing = true;

        while ((to_go.length > 0) && processing) {
            processing = false;
            const skipped = [];

            for (const blockId of to_go) {
                const block = this.blocks.get(blockId);

                if (block.container_id) {
                    // Note that we are not interested on dependencies not in
                    // the move. WE HAVE TO CHECK FOR `FALSE`, NOT FOR EXISTENCE
                    if (processedById[block.container_id] === false) {
                        skipped.push(blockId);
                        continue;
                    }
                }

                sortedByDep.push(blockId);
                processedById[blockId] = true;
                processing = true;
            }

            to_go = skipped;
        }

        if (to_go.length !== 0) {
            throw new Error("Found container-contained circular dependency, on the following IDs: " + JSON.stringify(to_go));
        }

        return sortedByDep;
    }

    private ensureBlockSelected(blockId: string) {
        if (this._selectedBlocks.indexOf(blockId) >= 0) {
            // It's already selected, nothing to do
            return;
        }
        else {
            // Update selection
            this.updateSelectBlockList([blockId]);
        }
    }

    private update_top_left() {
        const width = this.baseElement.clientWidth;
        const height = this.baseElement.clientHeight;

        this.canvas.setAttributeNS(null, 'viewBox',
                                   `${this.top_left.x} ${this.top_left.y} ${width * this.inv_zoom_level} ${height * this.inv_zoom_level}`);

        this.canvas.style.backgroundPosition = `${-this.top_left.x / this.inv_zoom_level}px ${-this.top_left.y / this.inv_zoom_level}px`;

        // Move trashcan
        const trashbox = this.trashcan.getElementsByTagName('image')[0].getBBox();
        if (trashbox) {
            // Move
            const left = width * this.inv_zoom_level - trashbox.width * this.inv_zoom_level + this.top_left.x;
            const top = height * this.inv_zoom_level - trashbox.height * this.inv_zoom_level + this.top_left.y;

            this.trashcan.setAttributeNS(null, 'transform', `matrix(${this.inv_zoom_level},0,0,${this.inv_zoom_level},${left},${top})`);

            // Move buttons
            {
                const buttonBox = this.button_group.getBBox();

                const left = width * this.inv_zoom_level - (buttonBox.width - FAB_BUTTON_PADDING) * this.inv_zoom_level + this.top_left.x;
                const top = height * this.inv_zoom_level - (trashbox.height + FAB_BUTTON_PADDING + buttonBox.height) * this.inv_zoom_level + this.top_left.y;

                this.button_group.setAttributeNS(null, 'transform', `matrix(${this.inv_zoom_level},0,0,${this.inv_zoom_level},${left},${top})`);
            }
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
            this.inv_zoom_level -= SMALL_ZOOM_INCREMENTS;
        }
        else {
            this.inv_zoom_level -= LARGE_ZOOM_INCREMENTS;
        }

        this.update_top_left();
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
            this.inv_zoom_level += SMALL_ZOOM_INCREMENTS;
        }
        else {
            this.inv_zoom_level += LARGE_ZOOM_INCREMENTS;
        }

        this.update_top_left();
    }

    private zoom_reset() {
        this.inv_zoom_level = 1;
        this.update_top_left(); // This has to be done before center() for it to work correctly

        this.center();
    }

    // Perform an operation while resetting the zoom level
    private _withNoZoom(f: () => void) {
        // Remove zoom
        const zoomLevel = this.inv_zoom_level;
        this.inv_zoom_level = 1;
        this.update_top_left();

        // Apply operation
        let error = null;
        let hadException = false;

        try {
            f();
        }
        catch (err) {
            error = err;
            hadException = true;
        }

        // Reset zoo
        this.inv_zoom_level = zoomLevel;
        this.update_top_left();

        // Re-throw exception if one was found
        if (hadException) {
            throw error;
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

    public getBlock(blockId: string): FlowBlock {
        if (!this.blockObjs[blockId]) {
            throw Error(`Block (id=${blockId}) not found`);
        }

        return this.blockObjs[blockId].block;
    }

    public drawAbsolute(block: FlowBlock, abs_position: Position2D): string {
        const canvas_area = this.canvas.getClientRects()[0];

        const rel_pos = {
            x: (abs_position.x - canvas_area.x) * this.inv_zoom_level + this.top_left.x,
            y: (abs_position.y - canvas_area.y) * this.inv_zoom_level + this.top_left.y,
        };

        return this.draw(block, rel_pos);
    }

    public draw(block: FlowBlock, position?: Position2D): string {
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

        if (!position) {
            position = {x: 10, y: 10};
        }

        this._withNoZoom(() => {
            block.render(group, {
                position: position,
                workspace: this,
            });
        });

        if (isContainer) {
            // Obtaining the area has to be done AFTER the rendering
            this.containers.push((block as FlowBlock & ContainerBlock));

            if ((block as ContainerFlowBlock).isPage) {
                this.numPages++;
            }
        }

        const bodyElement = block.getBodyElement();

        // Events are set even on read-only contexts, so in later iterations,
        // that property can be changed dynamically.
        bodyElement.oncontextmenu = (ev: MouseEvent) => {
            ev.preventDefault();

            if (this.read_only) {
                return;
            }

            this.showBlockContextMenu(this._getPositionFromEvent(ev));
        };

        let canBeMoved = true;
        if (block instanceof ContainerFlowBlock) {
            canBeMoved = !block.cannotBeMoved;
        }

        if (canBeMoved) {
            bodyElement.onmousedown = bodyElement.ontouchstart = ((ev: MouseEvent | TouchEvent) => {
                if (this.state !== 'waiting'){
                    return;
                }

                if (this.read_only) {
                    return;
                }

                if (this.current_io_selected) { return; }

                this.ensureContextMenuHidden();

                if ((ev as MouseEvent).button === 2) {
                    // On right click just make sure it is selected, the context
                    // menu will be handled by 'oncontextmenu'.
                    this.ensureBlockSelected(block.id);
                    // TODO: How to perform this action on touch event? Long touch?
                }
                else {
                    this._mouseDownOnBlock(this._getPositionFromEvent(ev), block);
                }
            });
        }

        const input_group = this.drawBlockInputHelpers(block);

        this.blockObjs[block.id] = { block: block, input_group: input_group };
        this.blocks.set(block.id, { connections: [],
                                    container_id: null,
                                    position: position,
                                    blockData: block.serialize(),
                                 })
        block.onMove((pos: Position2D) => {
            const data = this.blocks.get(block.id);
            data.position = pos;
            this.blocks.set(block.id, data);
        })

        return block.id;
    }

    public centerOnBlock(blockId: string) {
        const block = this.blockObjs[blockId].block;
        const area = block.getBodyArea();
        const centerX = area.x + area.width / 2;
        const centerY = area.y + area.height / 2;

        this.centerOnPoint({ x: centerX, y: centerY });
    }

    public centerOnPoint(pos: Position2D) {
        // Consider toolbox overlap
        let marginRight = 0;
        if (this.toolbox.blockShowcase){
            marginRight = this.toolbox.blockShowcase.getBoundingClientRect().width;
        }

        const width = this.canvas.width.baseVal.value - marginRight;
        const height = this.canvas.height.baseVal.value;

        this.top_left.x = (pos.x - width/2) - marginRight;
        this.top_left.y = pos.y - height/2;
        this.update_top_left();
    }

    public center() {
        // Find the center of all blocks, and center the view there
        const blockIds = Object.keys(this.blockObjs);
        if (blockIds.length === 0) {
            return this.centerOnPoint({ x: 0, y: 0 });
        }

        const block1Area = this.blockObjs[blockIds[0]].block.getBodyArea();
        const rect = {
            left: block1Area.x,
            top: block1Area.y,
            right: block1Area.x + block1Area.width,
            bottom: block1Area.y + block1Area.height,
        };

        for (let i = 1 ; i < blockIds.length; i++) {
            const blockArea = this.blockObjs[blockIds[i]].block.getBodyArea();

            if (blockArea.x < rect.left) {
                rect.left = blockArea.x;
            }
            if (blockArea.y < rect.top) {
                rect.top = blockArea.y;
            }

            const right = blockArea.x + blockArea.width;
            const bottom = blockArea.y + blockArea.height;
            if (right > rect.right) {
                rect.right = right;
            }
            if (bottom > rect.bottom) {
                rect.bottom = bottom;
            }
        }

        const center = {
            x: (rect.left + rect.right) / 2,
            y: (rect.top + rect.bottom) / 2,
        };

        return this.centerOnPoint(center);
    }

    public showBlockContextMenu(pos: Position2D) {
        // Base positioning
        this.popupGroup.innerHTML = '';
        this.popupGroup.setAttribute('class', 'popup_group context_menu');

        const canvas_rect = this.canvas.getClientRects()[0];
        const workspacePos = { x: pos.x - canvas_rect.x, y: pos.y - canvas_rect.y };

        this.popupGroup.style.left = workspacePos.x + 'px';
        this.popupGroup.style.top = workspacePos.y + 'px';
        delete this.popupGroup.style.maxHeight;

        // Block operations
        const block_ops = document.createElement('ul');

        // Default options
        if (this._selectedBlocks.some(this._canCloneBlock.bind(this))) {
            const clone_entry = document.createElement('li');
            clone_entry.innerText = 'Clone';
            clone_entry.onclick = (ev) => { this.ensureContextMenuHidden(); this.cloneSelection(); };

            block_ops.appendChild(clone_entry);
        }

        // Single block options
        if (this._selectedBlocks.length === 1) {
            const blockId = this._selectedBlocks[0];
            const block = this.blockObjs[blockId].block;
            const actions = block.getBlockContextActions();

            for (const action of actions) {
                const entry = document.createElement('li');
                entry.innerText = action.title;
                entry.onclick = (ev) => { this.ensureContextMenuHidden(); action.run(); };
                block_ops.appendChild(entry);
            }
        }

        this.popupGroup.appendChild(block_ops);
    }

    public cloneSelection(): string[] {
        const newIds = [];

        // Unselect blocks that cannot be cloned
        const blocks = this._selectedBlocks.filter(this._canCloneBlock.bind(this));

        for (const blockId of blocks) {

            const blockObj = this.blockObjs[blockId];
            const data = blockObj.block.serialize();

            const newId = uuidv4();
            const clone = this.deserializeBlock(newId, data);

            const prevPos = blockObj.block.getBodyArea();
            this.draw(clone, { x: prevPos.x + 20, y: prevPos.y - 20 });
            newIds.push(newId);
        }

        // Replicate connections among the selected blocks
        for (const connectionId of Array.from(this.connections.keys())) {
            const connection = this.connections.get(connectionId);

            // Look for matching sink
            const sink = connection.sink;
            const sinkIndex = blocks.indexOf(sink.block_id);
            if (sinkIndex < 0) {
                continue;
            }

            // Look for matching source
            const source = connection.source;
            const sourceIndex = blocks.indexOf(source.block_id);
            if (sourceIndex < 0) {
                continue;
            }

            this.establishConnection(
                {
                    block: this.blockObjs[newIds[sourceIndex]].block,
                    type: 'out',
                    index: connection.source.output_index,
                },
                {
                    block: this.blockObjs[newIds[sinkIndex]].block,
                    type: 'in',
                    index: connection.sink.input_index,
                },
            );
        }

        this.updateSelectBlockList(newIds);

        return newIds;
    }

    public ensureContextMenuHidden() {
        this.popupGroup.classList.remove('context_menu');
        this.popupGroup.classList.add('hidden');
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

            const newWidth = area.width   - diffX * this.inv_zoom_level;
            const newHeight = area.height - diffY * this.inv_zoom_level;

            block.resize({ width: newWidth, height: newHeight });
        });

        this.canvas.onmouseup = this.canvas.ontouchend = ((ev: MouseEvent | TouchEvent) => {
            this.canvas.onmousemove = null;
            this.canvas.onmouseup = null;
        });
    }

    private _canCloneBlock(blockId: string): boolean {
        const block = this.blockObjs[blockId].block;

        if (block instanceof ContainerFlowBlock) {
            if (block.isPage) {
                return false;
            }
        }

        return true;
    }

    private _findContainerInPos(pos: Position2D, excludingList: string[]): FlowBlock | null {
        const candidates: (ContainerBlock & FlowBlock)[] = [];

        for (const container of this.containers) {
            if (excludingList.indexOf(container.id) >= 0) {
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

                candidates.push(container);
            }
        }

        if (candidates.length === 0) {
            return null;
        }

        // Candidate priority
        //  1. First, the ones that are not pages
        //  2. The ones with smaller area
        const pages = [];
        const notPages = [];

        for (const container of candidates) {
            if (container.isPage) {
                pages.push(container);
            }
            else {
                notPages.push(container);
            }
        }

        const partition = notPages.length > 0 ? notPages : pages;
        partition.sort((a, b) => {
            const areaA = a.getBodyArea();
            const areaB = b.getBodyArea();

            return (areaA.height * areaA.width) - (areaB.height * areaB.width);
        });

        return partition[0];
    }

    private _getContainerOfBlock(blockId: string): FlowBlock | null {
        const blockInfo = this.blocks.get(blockId);
        if (!blockInfo) {
            throw new Error("Can't find block info of " + blockId);
        }
        if (blockInfo.container_id) {

            if (!blockInfo) {
                throw new Error("Can't find container: " + blockInfo.container_id);
            }

            return this.blockObjs[blockInfo.container_id].block;
        }

        return null;
    }

    private _updateBlockContainer(block: FlowBlock, container?: FlowBlock) {
        const block_id = block.id;
        const wasInContainer = this._getContainerOfBlock(block_id);
        this._updateBlockContainerFromContainer(block, container, wasInContainer);
    }

    private _updateBlockContainerFromContainer(block: FlowBlock, container?: FlowBlock, wasInContainer?: FlowBlock) {
        const block_id = block.id;

        const container_id = container ? container.id : null;

        if (wasInContainer !== container) {
            if (wasInContainer) {
                (wasInContainer as any as ContainerBlock).removeContentBlock(block);
            }

            if (container) {
                try {
                    (container as any as ContainerBlock).addContentBlock(block);
                }
                catch (err) {
                    if (err instanceof CannotSetAsContentsError) {
                        this.blocks.get(block_id).container_id = null;
                    }
                    throw err;
                }
            }

            if (this.blocks.has(block_id)) {
                const data = this.blocks.get(block_id);
                if (data.container_id !== container_id) {
                    data.container_id = container_id;
                    this.blocks.set(block_id, data);
                }
            }

            if (block instanceof UiFlowBlock) {
                block.updateContainer(container);
            }
        }
        else if (container) {
            (container as any as ContainerBlock).update();
        }
    }

    private _mouseDownOnBlock(pos: Position2D, block: FlowBlock, on_done?: (pos: Position2D) => void) {
        const block_id = block.id;
        this.ensureBlockSelected(block_id);

        if (this.state !== 'waiting') {
            console.error('Forcing start of MouseDown with Workspace state='+this.state);
        }
        this.state = 'dragging-block';

        const bodyElement = block.getBodyElement();

        let last = pos;
        let lastContainer: FlowBlock | null = null;

        this.canvas.onmousemove = this.canvas.ontouchmove = ((ev: MouseEvent | TouchEvent) => {
            const pos = this._getPositionFromEvent(ev);
            const container = this._findContainerInPos(pos, this._selectedBlocks.concat([block_id]));

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

                for (const blockId of this._selectedBlocks) {
                    const container = this._getContainerOfBlock(blockId);
                    const isContainerSelected = container === null ? false : this._selectedBlocks.indexOf(container.id) >= 0;
                    if (isContainerSelected) {
                        // Container of the block is also selected, avoid moving it twice
                        continue;
                    }

                    const draggedBlocks = this.blockObjs[blockId].block.moveBy(distance).map(block => block.id);
                    this._afterBlocksMove(draggedBlocks.concat([block.id]));
                }

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

            const oldContainer: string | null = this.blocks.get(block_id).container_id;
            const pos = this._getPositionFromEvent(ev);
            const container = this._findContainerInPos(pos, this._selectedBlocks);
            const containerId = container === null ? null : container.id;

            let moved: string[] = [];

            // Only update container if either:
            //  - The dragged block was in a container and now is not
            //  - The dragged block is dropped in a container not in the selected group
            if ((oldContainer && (!containerId))
                || (containerId && (this._selectedBlocks.indexOf(containerId) < 0))) {

                for (const blockId of this._selectedBlocks.concat([])) {
                    const blockInfo = this.blocks.get(blockId);
                    const blockObj = this.blockObjs[blockId];

                    try {
                        // Don't update container if it's on the selection
                        if (blockInfo.container_id && this._selectedBlocks.indexOf(blockInfo.container_id) >= 0) {
                            continue;
                        }

                        this._updateBlockContainer(blockObj.block, container);
                    }
                    catch (err) {
                        if (err instanceof CannotSetAsContentsError) {
                            console.error("Cannot set as content", err.problematicContents); // TODO: Show as notification
                            this._updateBlockContainer(blockObj.block, null);
                        }
                    }
                }
            }

            // Track the blocks dragged
            for (const blockId of this._selectedBlocks.concat([])) {
                const draggedBlocks = this.blockObjs[blockId].block.endMove().map(block => block.id);

                moved.push(blockId)
                moved = moved.concat(draggedBlocks);
            }

            let removed = false;
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
                    removed = true;
                    for (const blockId of moved.concat([])) {
                        this.removeBlock(blockId, this.blocks.get(blockId));
                    }
                }
            }
            catch (err) {
                console.error(err);
            }

            // If autoposition is not activated, only move the connections present
            if (!removed && !this.autoposition) {
                // Update moved block's connections
                for (const movedId of moved) {
                    for (const conn of this.blocks.get(movedId).connections) {
                        this.updateConnection(conn);
                    }

                    this.updateBlockInputHelpersPosition(movedId);
                }

                // Take into account the old container
                if (oldContainer) {
                    moved.push(oldContainer);
                }
            }

            // Else, just rely on the autopositioning
            if (this.autoposition) {
                this.repositionAll();
            }
        });
    }

    private _afterBlocksMove(blockIds: string[]) {
        for (const movedId of blockIds) {
            for (const conn of this.blocks.get(movedId).connections) {
                this.updateConnection(conn);
            }

            this.updateBlockInputHelpersPosition(movedId);
        }
    }

    public invalidateBlock(blockId: string) {
        this._invalidateBlockPositions([blockId]);
    }

    private _invalidateBlockPositions(blocks: string[]) {
        // This would be a good point to save the invalidated blocks and not
        // launch the repositioning in case the initial "build" is not finished
        if (this.autoposition) {
            this._reposition(blocks);
        }
    }

    public repositionAll() {
        const blocks = Object.keys(this.blockObjs);
        this._reposition(blocks);

        for (const blockId of blocks) {
            for (const conn of this.blocks.get(blockId).connections) {
                this.updateConnection(conn);
            }

            this.updateBlockInputHelpersPosition(blockId);
        }
    }

    async repositionIteratively(max_iterations?: number) {
        // For positioning debugging purposes
        const its = [];

        if (!max_iterations) {
            max_iterations = DEFAULT_MAX_ITERATIONS;
        }
        if (max_iterations > ABSOLUTE_MAX_ITERATIONS) {
            max_iterations = ABSOLUTE_MAX_ITERATIONS;
            console.warn(`Limited max iterations number. ${max_iterations} -> ${ABSOLUTE_MAX_ITERATIONS}`);
        }

        for (let i = 0; i < max_iterations; i++) {
            console.time("It " + (i + 1));

            const prevPos: [string, Area2D][] = Object.keys(this.blockObjs).map( id => [id, this.blockObjs[id].block.getBodyArea() ] )
            this.repositionAll();

            const diffs = prevPos.map(([id, prev]) => {
                const after = this.blockObjs[id].block.getBodyArea();

                return {
                    block: this.blockObjs[id].block,
                    x: Math.abs(after.x - prev.x),
                    y: Math.abs(after.y - prev.y),
                    width: Math.abs(after.width - prev.width),
                    height: Math.abs(after.height - prev.height),
                }
            })

            const mov = {
                x: maxKey(diffs, e => e.x),
                y: maxKey(diffs, e => e.y),
                width: maxKey(diffs, e => e.width),
                height: maxKey(diffs, e => e.height),
            };
            its.push(mov)
            console.timeEnd("It " + (i + 1));
            console.debug('Max movement in iteration:',
                          {
                x: { x: mov.x.x, block: mov.x.block },
                y: { y: mov.y.y, block: mov.y.block },
                width: { width: mov.width.width, block: mov.width.block },
                height: { height: mov.height.x, block: mov.height.block },
            });

            if ((Math.abs(mov.x.x) < 1) && (Math.abs(mov.y.y) < 1) && (Math.abs(mov.width.width) < 1) && (Math.abs(mov.height.height) < 1)) {
                console.log("Stable on it", i); // No +1 because it was already stable from last iteration
                break;
            }

            await new Promise(resolve => setTimeout(resolve, TIME_BETWEEN_POSITION_ITERATIONS));
        }

        return its;
    }

    private _reposition(blockIds: string[]) {
        this.doc.transact((_transaction: Y.Transaction) => {
            // Build the list of dependencies (contents) for each block repositioned
            const dependencies: {[key: string]: string[]} = {};

            const considered: {[key: string]: boolean} = {};
            for (const id of blockIds) {
                considered[id] = true;
            }

            const allAffected = [];
            const toExplore = blockIds.concat([]);
            while (toExplore.length > 0) {
                const id = toExplore.shift();
                allAffected.push(id);

                const block = this.blocks.get(id);

                if (block.container_id) {
                    const dep = block.container_id;
                    if (!considered[dep]) {
                        toExplore.push(dep);
                        considered[dep] = true;
                    }

                    if (!dependencies[dep]){
                        dependencies[dep] = [];
                    }

                    dependencies[dep].push(id);
                }
            }

            const processed: string[] = [];
            let toGo = allAffected.concat([]);
            let processedThisTurn = true;
            while (toGo.length > 0 && processedThisTurn) {
                processedThisTurn = false;
                const skipped = [];

                for (const bId of toGo) {
                    const blockObj = this.blockObjs[bId];
                    if ((dependencies[bId] || []).some(x => processed.indexOf(x) < 0)) {
                        // Not all contents have been repositioned yet
                        skipped.push(bId);
                    }
                    else {
                        const block = blockObj.block;
                        if (block instanceof ContainerFlowBlock) {
                            block.repositionContents();
                        }

                        processedThisTurn = true;
                        processed.push(bId);
                    }
                }

                toGo = skipped;
            }

            if (toGo.length > 0) {
                console.error("Circular dependency found on", toGo);
                console.error("Circular dependency found on", toGo.map(id => this.blocks.get(id)));
            }

            // After all are processed, give then the option to "settle" on their new position
            for (const elementId of processed.reverse()) {
                const block = this.blockObjs[elementId].block;

                // This have a reasonably-close semantic, but it might not be
                // enough. A new function might be needed to cover this meaning.
                block.endMove();
            }
        });
    }

    private _pullAllDependenciesInList(id: string, group: string[]): string[] {
        let deps: string[] = [];
        const block = this.blocks.get(id);

        if (block.container_id && group.indexOf(block.container_id) >= 0) {
            deps.push(block.container_id);

            const subdeps = this._pullAllDependenciesInList(block.container_id, group);

            if (subdeps.length > 0) {
                deps = deps.concat(subdeps);
            }
        }

        return deps;
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

        let inputs = block.getInputs();
        if (this.read_only) { inputs = []; }

        let index = -1;
        for (const input of inputs) {
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
                        this.createEnumValue(input, block.id, element_index, { position })
                    }
                    else {
                        this.createDirectValue(input.type, block.id, element_index, { position });
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
        }, uuidv4());

        const enum_input_id = this.draw(enum_input, options.position);
        this.addConnection({ block_id: enum_input_id, output_index: 0 },
                           { block_id: block_id, input_index: input_index });
    }

    private createDirectValue(type: MessageType, block_id: string, input_index: number,
                              options: { position: Position2D, value?: string }) {
        const direct_input = new DirectValue({ type: type,
                                               on_request_edit: this.onRequestEdit.bind(this),
                                               value: options.value,
                                               on_io_selected: this.onIoSelected.bind(this),
                                             }, uuidv4());

        const direct_input_id = this.draw(direct_input, options.position);
        this.addConnection({ block_id: direct_input_id, output_index: 0 },
                           { block_id: block_id, input_index: input_index });
    }

    private static MessageTypeToInputType(type: MessageType): string {
        if (!type) { type = 'any'; }

        switch(type) {
            case 'string':
            case 'any':
            case 'pulse':
            case 'user-pulse':
                return 'text';

            case 'float':
            case 'integer':
                return 'number';

            case 'boolean':
                return 'checkbox';
        }
    }

    public editInline(area: Area2D, value: string, type: MessageType, update: (value: string) => void): void {
        let editor: HTMLInputElement | HTMLTextAreaElement = null;
        let hiddenEditor = null;
        if (type === 'boolean') {
            editor = this.inlineEditor;
            hiddenEditor = this.inlineMultilineEditor;

            this.inlineEditor.step = '';
            this.inlineEditor.type = FlowWorkspace.MessageTypeToInputType(type);
        }
        else if (type === 'integer') {
            editor = this.inlineEditor;
            hiddenEditor = this.inlineMultilineEditor;

            this.inlineEditor.step = '1';
            this.inlineEditor.type = FlowWorkspace.MessageTypeToInputType(type);
        }
        else if (type === 'float') {
            editor = this.inlineEditor;
            hiddenEditor = this.inlineMultilineEditor;

            this.inlineEditor.step = '0.1';
            this.inlineEditor.type = FlowWorkspace.MessageTypeToInputType(type);
        }
        else {
            editor = this.inlineMultilineEditor;
            hiddenEditor = this.inlineEditor;
        }
        editor.value = value;

        if (editor === hiddenEditor) {
            throw Error("Hidden editor and used one should NOT be the same");
        }

        const valueArea = this.getWorkspaceRelArea(area);

        this.inlineEditorContainer.style.top = valueArea.y + 2 + 'px';
        this.inlineEditorContainer.style.left = valueArea.x + 'px';
        editor.style.width = valueArea.width + 'px';
        editor.style.height = valueArea.height - 4 + 'px';
        editor.style.fontSize = (1 / this.inv_zoom_level) * 100 + '%';

        this.inlineEditorContainer.classList.remove('hidden');
        editor.classList.remove('hidden');
        hiddenEditor.classList.add('hidden');

        const finishEdition = () => {
            editor.onblur = null;
            editor.onkeypress = null;
            this.inlineEditorContainer.classList.add('hidden');

            if (type === 'boolean') {
                update(this.inlineEditor.checked ? 'true' : 'false');
            }
            else {
                update(editor.value);
            }
        }

        editor.onblur = () => {
            finishEdition();
        };

        editor.onkeypress = (ev:KeyboardEvent) => {
            if ((ev.shiftKey) && (ev.key === 'Enter')) {
                finishEdition();
            }
        };

        editor.focus();
    }

    private updateBlockInputHelpersPosition(block_id: string) {
        const blockObj = this.blockObjs[block_id];

        // Deactivate helpers for all inputs in use
        let index = -1;
        for (const input of Array.from(blockObj.input_group.children)) {
            index++;

            const input_position = this.getBlockRel(blockObj.block, blockObj.block.getPositionOfInput(index));
            input.setAttributeNS(null, 'transform',
                                 `translate(${input_position.x - HELPER_BASE_SIZE / 2},`
                + `${input_position.y - HELPER_BASE_SIZE / 2 - HELPER_SEPARATION})`);
        }
    }

    private updateBlockInputHelpersVisibility(block_id: string) {
        const blockInfo = this.blocks.get(block_id);
        const blockObj = this.blockObjs[block_id];

        const inputs_in_use: {[key: string]: boolean} = {};
        for (const conn_id of blockInfo.connections) {
            const conn = this.connections.get(conn_id);

            if (conn.sink.block_id == block_id) {
                inputs_in_use[conn.sink.input_index] = true;
            }
        }

        // Deactivate helpers for all inputs in use
        let index = -1;
        for (const input of Array.from(blockObj.input_group.children)) {
            index++;

            if (inputs_in_use[index]) {
                input.classList.add('hidden');
            }
            else {
                input.classList.remove('hidden');
            }
        }
    }

    public get hasPages() {
        return this.numPages > 0;
    }

    public removeBlock(blockId: string, info?: SharedBlockData) {
        if (!info) {
            info = this.blocks.get(blockId);
        }

        const blockObj = this.blockObjs[blockId];
        console.debug("Removing block:", info);

        if (!blockObj) {
            console.debug("Already removed", blockId);

            return;
        }

        const slots = blockObj.block.getSlots();
        if (slots.variable) {
            if (this.variables_in_use[slots.variable]) {
                this.variables_in_use[slots.variable]--;
            }
        }


        if (blockObj.block instanceof ContainerFlowBlock) {
            const parent_container_id = info.container_id;
            const parent_container = parent_container_id ? this.blockObjs[parent_container_id].block : null;

            for (const content of blockObj.block.contents.concat([])) {
                try {
                    this._updateBlockContainer(content, parent_container);
                }
                catch (err) {
                    if (err instanceof CannotSetAsContentsError) {
                        this._updateBlockContainer(content, null); // Ignore container
                    }
                    else {
                        throw err;
                    }
                }
            }

            if (blockObj.block.isPage) {
                this.numPages--;
            }
        }

        if (this.blocks.has(blockId)) {
            this._updateBlockContainer(blockObj.block, null);
        }

        // Make a copy of the array to avoid problems for modifying it during the loop
        for (const conn_id of info.connections.concat([])) {
            this.removeConnection(this.connections.get(conn_id));
        }

        this.input_helper_section.removeChild(blockObj.input_group);
        blockObj.block.dispose();

        delete this.blockObjs[blockId];
        if (this.blocks.has(blockId)) {
            this.blocks.delete(blockId);
        }

        const idx = this._selectedBlocks.indexOf(blockId);
        if (idx >= 0) {
            this._selectedBlocks.splice(idx, 1);
        }
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

    private absPosToWorkspace(area: Area2D): Area2D {
        const canvas_rect = this.canvas.getClientRects()[0];
        return {
            x: ((area.x - canvas_rect.left) * this.inv_zoom_level) + this.top_left.x,
            y: ((area.y - canvas_rect.top) * this.inv_zoom_level) + this.top_left.y,
            width: area.width * this.inv_zoom_level,
            height: area.height * this.inv_zoom_level,
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

        for (const block_id of Object.keys(this.blockObjs)) {
            const block = this.blockObjs[block_id].block;
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

    addConnection(from_: SourceDefinition,
                  to: SinkDefinition,
                  ): boolean {

        if (!(from_.block_id in this.blockObjs) || !(to.block_id in this.blockObjs)) {
            console.error("Trying to create connection from non-spawned blocks",
                          {
                from: from_,
                to: to,
                from_exists: (from_.block_id in this.blockObjs),
                to_exists: to.block_id in this.blockObjs
            });

            return;
        }

        const sourceObj = this.blockObjs[from_.block_id];

        const source = this.blocks.get(from_.block_id);
        const source_output_type = sourceObj.block.getOutputType(from_.output_index);

        // The combination (output block&port) -> (input block&port) should be unique.
        const id = `${from_.block_id}:${from_.output_index}--${to.block_id}:${to.input_index}`;

        if (id in this.connectionElements) {
            console.debug("Connection already exists");
            return;
        }

        const path = document.createElementNS(SvgNS, 'path');

        const conn : FlowConnectionData = { id: id, source: from_, sink: to, type: source_output_type };

        if ((source_output_type == 'pulse') || (source_output_type == 'user-pulse')) {
            path.setAttributeNS(null, 'marker-end', 'url(#pulse_head)');
            path.onmouseenter = () => {
                path.setAttributeNS(null, 'marker-end', 'url(#pulse_head_selected)');
            };
            path.onmouseleave = () => {
                path.setAttributeNS(null, 'marker-end', 'url(#pulse_head)');
            };
        }

        setConnectionType(source_output_type, conn, path);
        path.onclick = () => {
            if (this.read_only) { return }

            this.removeConnection(conn);
        };
        this.connection_group.appendChild(path);

        const sinkObj = this.blockObjs[conn.sink.block_id];
        const sink = this.blocks.get(conn.sink.block_id);

        sourceObj.block.addConnection('out', conn.source.output_index, sinkObj.block, source_output_type);
        source.connections.push(conn.id);

        sink.connections.push(conn.id);
        const hasChanged = sinkObj.block.addConnection('in', conn.sink.input_index, sourceObj.block, source_output_type);

        this.connectionElements[conn.id] = path;

        if (!this.connections.has(conn.id)) {
            this.connections.set(conn.id, conn);
        }
        this.updateBlockInputHelpersVisibility(conn.sink.block_id);

        if (hasChanged) {
            this.propagateChangesFrom(conn.sink.block_id);
        }

        this.updateConnection(conn.id);

        return true;
    }

    private removeConnection(conn: FlowConnectionData) {
        const sourceObj = this.blockObjs[conn.source.block_id];
        const sinkObj = this.blockObjs[conn.sink.block_id];

        const source = this.blocks.get(conn.source.block_id);
        const sink = this.blocks.get(conn.sink.block_id);

        // Disconnect from source
        sourceObj?.block.removeConnection('out', conn.source.output_index, sinkObj.block);
        if (source) {
            const source_conn_index = source.connections.indexOf(conn.id);
            if (source_conn_index < 0) {
                console.error('Connection not found when going to remove. For block', source);
            }
            else {
                source.connections.splice(source_conn_index, 1);
            }

            this.updateBlockInputHelpersVisibility(conn.source.block_id);
        }

        // Disconnect from sink
        const hasChanged = sinkObj?.block.removeConnection('in', conn.sink.input_index, sourceObj.block);
        if (sink) {
            const sink_conn_index = sink.connections.indexOf(conn.id);
            if (sink_conn_index < 0) {
                console.error('Connection not found when going to remove. For block', sink);
            }
            else {
                sink.connections.splice(sink_conn_index, 1);
            }
            this.updateBlockInputHelpersVisibility(conn.sink.block_id);
        }

        // Remove workspace information
        this.connection_group.removeChild(this.connectionElements[conn.id]);

        delete this.connectionElements[conn.id];
        if (this.connections.has(conn.id)) {
            this.connections.delete(conn.id);
        }

        if (hasChanged) {
            this.propagateChangesFrom(conn.sink.block_id);
        }
    }

    private propagateChangesFrom(originBlockId: string) {
        const considered: {[key: string]: boolean} = {};
        considered[originBlockId] = true;

        const todo = [originBlockId];

        while (todo.length > 0) {
            const next = todo.pop();
            const info = this.blocks.get(next);
            const blockObj = this.blockObjs[next];

            const linksFrom: [FlowConnectionData, SVGElement][] = [];
            const linksTo: [FlowConnectionData, SVGElement][] = [];

            // Explore where does this block lead to
            for (const connId of info.connections) {
                const connection = this.connections.get(connId);
                if (connection.source.block_id === next) {
                    const sink = connection.sink.block_id;
                    linksFrom.push([this.connections.get(connId), this.connectionElements[connId]]);

                    if (!considered[sink]) {
                        considered[sink] = true;
                        todo.push(sink);
                    }
                }
                else {
                    linksTo.push([this.connections.get(connId), this.connectionElements[connId]]);
                }
            }

            // Consider changes needed
            // *Right now* only AtomicFlowBlocks need this
            // TODO: Extend this to all blocks when type propagation is applied to more block types
            if (blockObj.block instanceof AtomicFlowBlock) {
                blockObj.block.refreshConnectionTypes(linksFrom, linksTo);
            }
        }
    }

    updateConnection(connection_id: string) {
        const conn = this.connections.get(connection_id);
        const runway = 20;

        // Source
        const source = conn.source;
        const source_block = this.blockObjs[source.block_id].block;

        const source_position = this.getBlockRel(source_block, source_block.getPositionOfOutput(source.output_index));

        // Sink
        const sink = conn.sink;
        const sink_block = this.blockObjs[sink.block_id].block;

        const element = this.connectionElements[connection_id];
        const connector_with_marker = !!element.getAttributeNS(null, 'marker-end');
        const y_sink_offset = connector_with_marker ? 2 : 0;

        const sink_position = this.getBlockRel(sink_block, sink_block.getPositionOfInput(sink.input_index, connector_with_marker));
        sink_position.y -= y_sink_offset;

        // Draw
        this.drawPath(element, source_position, sink_position, runway, source_block, sink_block);
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

        return this.addConnection({block_id: source.block.id, output_index: source.index },
                                  {block_id: sink.block.id, input_index: sink.index });
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

        const block_id = block.id;
        this.drawBlockInputHelpers(block, this.blockObjs[block_id].input_group);
    }

    onIoSelected(block: FlowBlock,
                 type: 'in'|'out',
                 index: number,
                 definition: InputPortDefinition | OutputPortDefinition,
                 port_center: Position2D,
                ): void {
        if (this.read_only) { return; }

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
        if (this.read_only) { return; }

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
            this._notifyChangedVariable(previous_value, val);

            update(val);
        };


        const MAX_RESULTS = 100; // Max. results to show at a single time
        const TIME_WAIT_FOR_SEARCH_TIME = 200; // Time to wait for next input before attempting to search
        const SCROLL_OPTIONS: ScrollIntoViewOptions = {
            behavior: 'smooth',
            block: 'nearest',
        };

        let bounce_control: NodeJS.Timeout = null;
        let last_query: string = null;

        let selected_index: number = null;

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
        if (this.read_only) { return; }

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
            this._notifyChangedVariable(previous_value, val);

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

    _notifyChangedVariable(prevValue: string, newValue: string) {
        if (this.variables_in_use[prevValue]) {
            this.variables_in_use[prevValue]--;
        }

        if (!this.variables_in_use[newValue]) {
            this.variables_in_use[newValue] = 0;
        }
        this.variables_in_use[newValue]++;
    }

    onRequestEdit(block: DirectValue, type: MessageType, update: (value: string) => void): void {
        this.editInline(block.getValueArea(), block.value, type, update);
    }
    // </Block manager interface>

    // Block configuration
    startBlockConfiguration(block: ConfigurableBlock) {
        const dialogRef = this.dialog.open(ConfigureBlockDialogComponent, {
            data: { block: block, programId: this.programId }
        });

        dialogRef.afterClosed().subscribe(async (result) => {
            if (!(result && result.success)) {
                console.log("Cancelled");
                return;
            }

            block.applyConfiguration((result.settings as BlockConfigurationOptions));
        });
    }

    getAssetUrlOnProgram(assetId: string): string {
        return this.programService.getAssetUrlOnProgram(assetId, this.programId);
    }
}
