import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock, Position2D, ManipulableArea2D } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockBuilder, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { HandleableElement, UiElementHandle } from "./ui_element_handle";
import { CutElement, CutNode, CutTree, CutType, UiElementRepr, ContainerElementRepr, DEFAULT_CUT_TYPE } from "./ui_tree_repr";
import { combinedArea, combinedManipulableArea, getRefBox, listToDict, manipulableAreaToArea2D } from "./utils";
import { PositionResponsiveContents, SEPARATION } from "./positioning";


const SvgNS = "http://www.w3.org/2000/svg";
const Title = "Responsive page";
const TITLE_PADDING = 5;

export const MIN_WIDTH = 200;
export const MIN_HEIGHT = 400;

export const ResponsivePageBuilder : ContainerFlowBlockBuilder = (canvas: SVGElement,
                                                                  group: SVGElement,
                                                                  block: ContainerFlowBlock,
                                                                  service: UiSignalService,
                                                                  initOps: UiFlowBlockBuilderInitOps,
                                                                 ) => {
    const element = new ResponsivePage(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class ResponsivePage implements ContainerFlowBlockHandler, HandleableElement, TextEditable {
    subscription: Subscription;
    textBox: SVGTextElement;
    handle: UiElementHandle | null = null;
    node: SVGGElement;
    textDim: { width: number; height: number; };
    rect: SVGRectElement;
    rectShadow: SVGRectElement;
    grid: SVGGElement;
    width: number;
    height: number;
    titleBox: SVGRectElement;
    title: string;
    contents: FlowBlock[] = [];

    constructor(canvas: SVGElement, group: SVGElement,
                public block: ContainerFlowBlock,
                private service: UiSignalService,
                private initOps: UiFlowBlockBuilderInitOps) {

        const refBox = getRefBox(canvas);

        this.titleBox = document.createElementNS(SvgNS, 'rect');
        this.node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node container_node responsive_page');

        this.grid = document.createElementNS(SvgNS, 'g');

        this.textBox = document.createElementNS(SvgNS, 'text');
        this.textBox.setAttribute('class', 'output_text');
        this.textBox.setAttributeNS(null,'textlength', '100%');

        this.textBox.textContent = this.title = block.blockData.textContent ||  Title;

        this.node.appendChild(this.rectShadow);
        this.node.appendChild(this.rect);
        this.node.appendChild(this.grid);
        this.node.appendChild(this.titleBox);

        this.node.appendChild(this.textBox);
        group.appendChild(this.node);

        const text_width = this.textBox.getClientRects()[0].width;
        const text_height = this.textBox.getClientRects()[0].height;
        this.textDim = { width: text_width, height: text_height };

        const bdims = block.blockData.dimensions;
        this.width = bdims ? bdims.width : text_width * 4;
        this.height = bdims ? bdims.height : refBox.height * 30;

        this.textBox.setAttributeNS(null, 'y', this.height/2 - text_height / 2 + "");
        this.textBox.setAttributeNS(null, 'x', (this.width - text_width)/2 + "");

        this.titleBox.setAttributeNS(null, 'class', 'titlebox');
        this.titleBox.setAttributeNS(null, 'x', "0");
        this.titleBox.setAttributeNS(null, 'y', "0");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");

        this.grid.setAttribute('class', 'division_grid');

        this.updateSizes();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, this.node, initOps.workspace, []);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }

    // Resizeable
    resize(dim: Area2D) {
        const baseWidth = Math.max(MIN_WIDTH, dim.width);
        const baseHeight = Math.max(MIN_HEIGHT, dim.height);

        const off = this.block.getOffset();
        let right = off.x + baseWidth;
        let bottom = off.y + baseHeight;

        const contents = this.block.recursiveGetAllContents();
        for (const c of contents) {

            if (! (c instanceof UiFlowBlock)) {
                continue;
            }

            const bArea = c.getBodyArea();

            let separationX = SEPARATION;
            const separationY = SEPARATION;

            if (c.isAutoresizable()) {
                const minArea = c.getMinSize();

                bArea.width = minArea.width;
                bArea.height = minArea.height;

                if (!c.isHorizontallyStackable()) {
                    separationX = 0
                }
            }

            const bRight  = bArea.x + bArea.width  + separationX;
            const bBottom = bArea.y + bArea.height + separationY;

            if (bRight > right) {
                right = bRight;
            }
            if (bBottom > bottom) {
                bottom = bBottom;
            }
        }

        const newWidth = (right - off.x);
        const newHeight = (bottom - off.y);

        const diffWidth = this.width - newWidth;
        const diffHeight = this.height - newHeight;

        this.width = newWidth;
        this.height = newHeight;

        this.updateSizes();

        (this.block as ContainerFlowBlock).update();
        this.handle.update();

        for (const content of this.contents) {
            if (content instanceof ContainerFlowBlock) {
                content.updateContainer(this.block);
            }
            else if ((content instanceof UiFlowBlock) && content.isAutoresizable()) {
                content.updateContainer(this.block);
            }
        }
    }

    updateSizes() {
        const text_width = this.textBox.getClientRects()[0].width;
        const text_height = this.textBox.getClientRects()[0].height;
        this.textDim = { width: text_width, height: text_height };

        const titleHeight = this.textDim.height + TITLE_PADDING * 2;
        this.titleBox.setAttributeNS(null, 'width', this.width + '');
        this.titleBox.setAttributeNS(null, 'height', titleHeight + "")

        this.rect.setAttributeNS(null, 'width', this.width + '');
        this.rect.setAttributeNS(null, 'height', this.height + '');

        this.rectShadow.setAttributeNS(null, 'width', this.width + '');
        this.rectShadow.setAttributeNS(null, 'height', this.height + '');

        this.textBox.setAttributeNS(null, 'x', (this.width - this.textDim.width)/2 + "");
        this.textBox.setAttributeNS(null, 'y', this.textDim.height + "");

        this.block.blockData.dimensions = { width: this.width, height: this.height };
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    // UiFlowBlock
    onClick() {
        this.block.startEditing();
    }

    onGetFocus() {
        if (!this.handle) {
            throw new Error("Cannot show manipulators as workspace has not been received.");
        }
        else {
            this.handle.show();
        }
    }

    onLoseFocus() {
        if (!this.handle) {
            throw new Error("Cannot show manipulators as workspace has not been received.");
        }
        else {
            this.handle.hide();
        }
    }

    isTextEditable(): this is TextEditable {
        return true;
    }

    isTextReadable(): this is TextReadable {
        return true;
    }


    get isStaticText(): boolean {
        return true;
    }

    get editableTextName(): string {
        return 'title';
    }

    public get text(): string {
        return this.title;
    }

    public set text(val: string) {
        this.textBox.textContent = this.block.blockData.textContent = this.title = val;
        this.updateSizes();
    }

    // Text edition area
    getArea(): Area2D {
        return this.titleBox.getBBox();
    }

    dispose() {}

    onInputUpdated(connectedBlock: FlowBlock, inputIndex: number) {}

    onConnectionLost(portIndex: number) {}

    onConnectionValueUpdate(_inputIndex: number, value: string) {}

    // Container element
    getBodyElement(): SVGRectElement {
        return this.rect;
    }

    getBlock(): FlowBlock {
        return this.block;
    }

    onContentUpdate(contents: FlowBlock[]) {
        // Obtain new distribution
        this.contents = contents.concat([]);
    }

    _updateCutGrid() {
        const uiContents = this.contents.filter(b => (b instanceof UiFlowBlock)) as UiFlowBlock[];

        // Update grid
        try {
            const tree = ResponsivePageGenerateTree(this, this.contents);

            this.grid.innerHTML = ''; // Clear it
            const cuts = performCuts(tree, uiContents, this.width, this.height, this.block.getOffset());

            for (const cut of cuts) {
                const path = document.createElementNS(SvgNS, 'path');
                path.setAttribute('class', `grid-division ${cut.type}-cut`);
                path.setAttributeNS(null, 'd', `M${ cut.from.x },${cut.from.y} L${cut.to.x},${cut.to.y}`);
                this.grid.appendChild(path);
            }
        }
        catch(err) {
            console.error(err);
            this.grid.innerHTML = ''; // Make sure it's clear
        }
    }

    dropOnEndMove() {
        this._updateCutGrid();

        return {x: 0, y: 0};
    }

    updateContainer(_container: UiFlowBlock) {
        throw new Error("A webpage cannot be put inside a container.");
    }

    repositionContents(): void {
        const area = this.getBodyArea();

        const titleHeight = this.titleBox.getBBox().height;
        area.y += titleHeight;

        const allContents = this.block.recursiveGetAllContents();

        const cutTree = PositionResponsiveContents(this, this.contents, allContents, area);

        const contentDict = listToDict(
            allContents.filter(x => x instanceof UiFlowBlock) as UiFlowBlock[],
            c => c.id);

        const elems = getElementsInGroup(cutTree)
            .map(id => contentDict[id])
            .filter(b => !b.isAutoresizable());

        const newArea = getRect(elems);

        this.resize(manipulableAreaToArea2D(newArea));
    }

    get container(): ContainerFlowBlock {
        return null;
    }

    update() {
        this.onContentUpdate(this.contents);
    }
}

type GridCut = { from: Position2D, to: Position2D, type: 'vert' | 'horiz' };

function performCuts(tree: CutTree, contents: UiFlowBlock[], width: number, height: number, offset: Position2D
                    ): GridCut[] {
    const acc: GridCut[] = [];
    let todo = [{ tree: tree, area: { x: 0, y: 0, width, height } }];

    const blocks = {};
    for (const block of contents) {
        blocks[block.id] = block;
        if (block instanceof ContainerFlowBlock) {
            for (const subBlock of block.recursiveGetAllContents()) {
                if (subBlock instanceof UiFlowBlock) {
                    blocks[subBlock.id] = subBlock;
                }
            }
        }
    }

    while (todo.length > 0) {
        const cut = todo.pop();

        if (!cut.tree) {
            // Empty group
            continue;
        }

        if ((cut.tree as UiElementRepr).widget_type) {
            continue; // Single element, nothing to cut
        }
        else if ((cut.tree as ContainerElementRepr).container_type) {
            continue; // Visual container, nothing to cut
        }

        const cTree = cut.tree as CutNode;
        const elements = cTree.groups.map(g => getElementsInGroup(g));
        const cGroups = elements.map((_value, idx) => idx).filter(idx => elements[idx].length > 0);

        if (cTree.cut_type === 'vbox') {
            // This cloning is probably not needed. Done now for simplicity.
            const availArea = Object.assign({}, cut.area);

            // Analyze as group pairs
            for (let i = 1; i < cGroups.length; i++) {


                const r1 = getRect(elements[cGroups[i - 1]].map(e => blocks[e]));
                const r2 = getRect(elements[cGroups[i]].map(e => blocks[e]));

                const cutYPos = (r1.bottom + (r2.top - r1.bottom) / 2) - offset.y;

                acc.push({ from: { x: availArea.x, y: cutYPos }, to: { x: availArea.x + availArea.width, y: cutYPos }, type: 'vert' });

                const topArea = Object.assign({}, availArea);
                topArea.height = cutYPos - availArea.y;
                todo.push({ tree: cTree.groups[cGroups[i - 1]], area: topArea });

                availArea.y = cutYPos;
                availArea.height -= topArea.height;

                if ((i + 1) === cGroups.length) {
                    // Only recurse bottom on the latest group, to avoid duplicating
                    todo.push({ tree: cTree.groups[cGroups[i]], area: availArea });
                }
            }
        }
        else if (cTree.cut_type === 'hbox') {
            // This cloning is probably not needed. Done now for simplicity.
            const availArea = Object.assign({}, cut.area);

            // Analyze as group pairs
            for (let i = 1; i < cGroups.length; i++) {

                const r1 = getRect(elements[cGroups[i - 1]].map(e => blocks[e]));
                const r2 = getRect(elements[cGroups[i]].map(e => blocks[e]));

                const cutXPos = (r1.right + (r2.left - r1.right) / 2) - offset.x;

                acc.push({ from: { x: cutXPos, y: availArea.y }, to: { x: cutXPos, y: availArea.y + availArea.height }, type: 'horiz' });

                const leftArea = Object.assign({}, availArea);
                leftArea.width = cutXPos - availArea.x;
                todo.push({ tree: cTree.groups[cGroups[i - 1]], area: leftArea });

                availArea.x = cutXPos;
                availArea.width -= leftArea.width;

                if ((i + 1) === cGroups.length) {
                    // Only recurse right on the latest group, to avoid duplicating
                    todo.push({ tree: cTree.groups[cGroups[i]], area: availArea });
                }
            }
        }
        else {
            throw new Error("Unknown cut type: " + cTree.cut_type);
        }
    }

    return acc;
}

export function getElementsInGroup(tree: CutTree): string[] {
    let acc = [];

    const todo = [tree];
    while (todo.length > 0) {
        const cut = todo.pop();

        if (!cut) {
            continue;
        }

        if ((cut as UiElementRepr).widget_type) {
                acc.push((cut as UiElementRepr).id);
        }
        else if ((cut as CutNode).groups) {
            if ((cut as CutNode).block_id) {
                acc.push((cut as CutNode).block_id);
            }
            for (const group of (cut as CutNode).groups) {
                todo.push(group);
            }
        }
        else if ((cut as ContainerElementRepr).container_type) {
            if ((cut as ContainerElementRepr).id) {
                acc.push((cut as ContainerElementRepr).id);
            }

            todo.push((cut as ContainerElementRepr).content);
        }
        else {
            console.warn("Unexpected node:", cut);
            throw Error("Unexpected node: "  + cut);
        }
    }

    return acc;
}

export function getShallowElementsInGroup(tree: CutTree): string[] {
    let acc = [];

    const todo = [tree];
    while (todo.length > 0) {
        const cut = todo.pop();

        if (!cut) {
            continue;
        }

        if ((cut as UiElementRepr).widget_type) {
            acc.push((cut as UiElementRepr).id);
        }
        else if ((cut as CutNode).groups) {
            if ((cut as CutNode).block_id) {
                acc.push((cut as CutNode).block_id);
            }
        }
        else if ((cut as ContainerElementRepr).container_type) {
            acc.push((cut as ContainerElementRepr).id);
        }
        else {
            console.warn("Unexpected node:", cut);
            throw Error("Unexpected node: "  + cut);
        }
    }

    return acc;
}

export function getRect(blocks: UiFlowBlock[]) {
    return combinedManipulableArea(blocks.map(b => b.getBodyArea()));
}

export const ResponsivePageGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    // Format in a grid-like
    const uiPos = (blocks
        .filter(b => (b instanceof UiFlowBlock))
        .map((b, i) => {
            return {i, a: b.getBodyArea(), b: (b as UiFlowBlock)};
        }));

    if (uiPos.length < 1) {
        return null;
    }
    if (uiPos.length < 2) {
        return uiPos[0].b.renderAsUiElement();
    }

    const tree = cleanestTree(uiPos, uiPos.map(({b: block}) => block));

    return reduceTree(tree);
}

// These two "reduce" functions might be merged into a single one. It's just not
// a priority right now, but it might be interesting to do it to check if it
// results on simpler code.
export function safeReduceTree(tree: CutTree) {
    return _reduceTree(tree, true);
}

function reduceTree(tree: CutTree): CutTree {
    return _reduceTree(tree, false);
}

function _reduceTree(tree: CutTree, safe: boolean): CutTree {
    if (!((tree as CutNode).cut_type)) {
        return tree;
    }

    const cNode = tree as CutNode;
    const newGroups = _reduceGroups(cNode, safe);

    const recasted: CutNode = { cut_type: cNode.cut_type, groups: newGroups };
    if (cNode.settings) {
        recasted.settings = cNode.settings;
    }
    if (cNode.block_id) {
        recasted.block_id = cNode.block_id;
    }
    return recasted;
}

function _reduceGroups(cNode: CutNode, safe: boolean): CutTree[] {
    let acc = [];
    const cType = cNode.cut_type;

    const aux = (tree: CutTree) => {
        if (!(tree as CutNode).cut_type) {
            acc.push(tree);
            return;
        }

        const cTree = tree as CutNode;

        // Trees and nodes with settings are not merged to avoid losing "colors" in the process
        let canMerge = ((!(cNode.settings && cNode.settings.bg))
            && (cTree.cut_type === cType) // Cut type must be the same
            && (!(cTree.settings && cTree.settings.bg)));

        if (canMerge && safe) {
            // Additional checks:
            //  - Neither of the blocks can have an ID
            canMerge = canMerge && (!cNode.block_id) && (!cTree.block_id);
        }

        if (canMerge) {
            for (const group of cTree.groups) {
                aux(group);
            }
        }
        else {
            acc.push(_reduceTree(cTree, safe));
        }
    }

    for (const group of cNode.groups) {
        aux(group);
    }

    return acc;
}

// Recursively perform cleanestCut, until all elements are partitioned.
export function cleanestTree(elems: CutElement[], blocks: UiFlowBlock[]): CutTree {
    const topLevel = [];

    const todo = [ { container: topLevel, elems: elems } ]

    let opNum = -1;

    // Easy limit for test, if the function works correctly it should neverbe reached.
    // Length * 2 should be enough, but some slack is allowed, given that this is only an intuition.
    let maxOps = elems.length * 3;

    while (todo.length > 0) {
        // We process items in the same order as they are generated to respect the group order
        const next = todo.shift();

        opNum++;
        if (opNum > maxOps) {
            throw new Error('Infinite loop found tree-ifying.'
                + ` Started with ${elems.length} elements, did ${opNum} cuts`
                +  ` and ${todo.length + 1} items remain.`);
        }

        let result : CutTree;
        if (next.elems.length < 1) {
            throw new Error(`Cannot build tree with < 1 element, found ${next.elems.length}`);
        }
        else if (next.elems.length === 1) {
            const block = blocks[next.elems[0].i];
            result = block.renderAsUiElement();
        }
        else {
            const cut = cleanestCut(next.elems);

            const resultGroups = [];
            result = { cut_type: cut.cutType, groups: resultGroups };
            for (const g of cut.groups){
                if (g.length > 0) {
                    todo.push({ container: resultGroups, elems: g });
                }
            }
        }

        next.container.push(result);
    }

    return topLevel[0];
}

// Perform a single cut that divides the elements on the place where there is more empty space.
function cleanestCut(elems: CutElement[]): { cutType: CutType, groups: CutElement[][] } {
    // Sort horizantally and vertically
    const horiz = elems.map(e => {
        if (e.b.isHorizontallyStackable()) {
            return e;
        }
        else {
            return {
                b: e.b,
                i: e.i,
                a: {
                    y: e.a.y,
                    height: e.a.height,
                    // Don't stack horizontally
                    x: -Infinity,
                    width: Infinity,
                }
            }
        }
    });
    horiz.sort((a, b) => a.a.x - b.a.x );

    const vert = elems.concat([]);
    vert.sort((a, b) => a.a.y - b.a.y );

    // Measure horizontal spaces
    const horizSpaces = [];
    let endX = null;
    for (let idx = 0; idx < horiz.length; idx++) {
        const e = horiz[idx];

        if (endX === null) {
            endX = e.a.x + e.a.width;
            horizSpaces.push([ -Infinity, idx, e ]);
            continue;
        }

        const diff = e.a.x - endX;
        endX = e.a.x + e.a.width;

        horizSpaces.push([ diff, idx, e ]);
    }

    horizSpaces.sort(([a, aIdx], [b, bIdx]) => {
        if (b != a) {
            return b - a
        }
        else {
            // If the biggest gap cannot be found, avoid using the first gap
            // (between nothing and the first element) as cut point.
            // To do this, for gaps with the same size, put the ones with higher "index" first.
            return bIdx - aIdx;
        }
    })

    // Measure vertical spaces
    const vertSpaces = [];
    let endY = null;
    for (let idx = 0; idx < vert.length; idx++) {
        const e = vert[idx];

        if (endY === null) {
            endY = e.a.y + e.a.height;
            vertSpaces.push([ -Infinity, idx, e ]);
            continue;
        }

        const diff = e.a.y - endY;
        endY = e.a.y + e.a.height;

        vertSpaces.push([ diff, idx, e ]);
    }

    vertSpaces.sort(([a, aIdx], [b, bIdx]) => {
        if (b != a) {
            return b - a
        }
        else {
            // If the biggest gap cannot be found, avoid using the first gap
            // (between nothing and the first element) as cut point.
            // To do this, for gaps with the same size, put the ones with higher "index" first.
            return bIdx - aIdx;
        }
    })

    // Find how to cut, horizontally or vertically
    let cutType : CutType = DEFAULT_CUT_TYPE;
    if (horizSpaces.length < 1) {
        if (vertSpaces.length < 1) {
            cutType = DEFAULT_CUT_TYPE;
        }
        else {
            cutType = 'vbox';
        }
    }
    else if (vertSpaces.length < 1) {
        cutType = 'hbox';
    }
    else {
        const maxHoriz = horizSpaces[0][0];
        const maxVert = vertSpaces[0][0];

        if (maxHoriz > maxVert) {
            cutType = 'hbox';
        }
        else {
            cutType = 'vbox';
        }
    }

    // Perform the cut on the index with the most space
    let before: CutElement[], after:CutElement[];
    if (cutType === 'hbox') {
        const cutIdx = horizSpaces[0][1];
        before = horiz.slice(0, cutIdx);
        after = horiz.slice(cutIdx);
    }
    else {
        const cutIdx = vertSpaces[0][1];
        before = vert.slice(0, cutIdx);
        after = vert.slice(cutIdx);
    }

    if((before.length === 0) || (after.length === 0)) {
        throw Error(`Splitting with no elements on one side (${before.length} -split- ${after.length})`);
    }

    return { cutType: cutType, groups: [before, after] };
}
