import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock, Resizeable, Position2D } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockBuilderInitOps, UiFlowBlockHandler, TextEditable, TextReadable } from "../ui_flow_block";
import { ContainerElement, ContainerElementHandle } from "./container_element_handle";
import { CutElement, CutNode, CutTree, CutType, UiElementRepr } from "./ui_tree_repr";
import { getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const Title = "Responsive page";

export const ResponsivePageBuilder : UiFlowBlockBuilder = (canvas: SVGElement,
                                                           group: SVGElement,
                                                           block: UiFlowBlock,
                                                           service: UiSignalService,
                                                           initOps: UiFlowBlockBuilderInitOps,
                                                          ) => {
    const element = new ResponsivePage(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class ResponsivePage implements ContainerFlowBlockHandler, ContainerElement, Resizeable {
    subscription: Subscription;
    text: SVGTextElement;
    handle: ContainerElementHandle | null = null;
    node: SVGAElement;
    textDim: { width: number; height: number; };
    rect: SVGRectElement;
    rectShadow: SVGRectElement;
    grid: SVGGElement;
    width: number;
    height: number;

    constructor(canvas: SVGElement, group: SVGElement,
                public block: UiFlowBlock,
                private service: UiSignalService,
                private initOps: UiFlowBlockBuilderInitOps) {

        const refBox = getRefBox(canvas);

        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node container_node responsive_page');

        this.grid = document.createElementNS(SvgNS, 'g');

        this.text = document.createElementNS(SvgNS, 'text');
        this.text.setAttribute('class', 'output_text');
        this.text.setAttributeNS(null,'textlength', '100%');

        this.text.textContent = Title;

        this.node.appendChild(this.rectShadow);
        this.node.appendChild(this.rect);
        this.node.appendChild(this.grid);

        this.node.appendChild(this.text);
        group.appendChild(this.node);

        const text_width = this.text.getClientRects()[0].width;
        const text_height = this.text.getClientRects()[0].height;
        this.textDim = { width: text_width, height: text_height };

        const bdims = block.blockData.dimensions;
        this.width = bdims ? bdims.width : text_width * 1.25;
        this.height = bdims ? bdims.height : refBox.height * 10;

        this.text.setAttributeNS(null, 'y', this.height/2 - text_height / 2 + "");
        this.text.setAttributeNS(null, 'x', (this.width - text_width)/2 + "");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'height', this.height + "");
        this.rect.setAttributeNS(null, 'width', this.width + "");

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'height', this.height + "");
        this.rectShadow.setAttributeNS(null, 'width', this.width + "");

        this.grid.setAttribute('class', 'division_grid');

        if (initOps.workspace) {
            this.handle = new ContainerElementHandle(this, initOps.workspace);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }

    // Resizeable
    resize(dim: { width: number; height: number; }) {
        this.width = Math.max(this.textDim.width, dim.width);
        this.height = Math.max(this.textDim.height, dim.height);

        this.rect.setAttributeNS(null, 'width', this.width + '');
        this.rect.setAttributeNS(null, 'height', this.height + '');

        this.rectShadow.setAttributeNS(null, 'width', this.width + '');
        this.rectShadow.setAttributeNS(null, 'height', this.height + '');

        this.text.setAttributeNS(null, 'x', (this.width - this.textDim.width)/2 + "");
        this.text.setAttributeNS(null, 'y', (this.height/2 + this.textDim.height/2) + "");

        this.block.blockData.dimensions = { width: this.width, height: this.height };

        (this.block as ContainerFlowBlock).update();
        this.handle.update();
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    // UiFlowBlock
    onClick() {
        if (!this.handle) {
            throw new Error("Cannot show manipulators as workspace has not been received.");
        }
        else {
            this.handle.show();
        }
    }

    isTextEditable(): this is TextEditable {
        return false;
    }

    isTextReadable(): this is TextReadable {
        return false;
    }

    dispose() {}

    onInputUpdated(connectedBlock: FlowBlock, inputIndex: number) {}

    onConnectionLost(portIndex: number) {}

    onConnectionValueUpdate(_inputIndex: number, value: string) {}

    // Container element
    getBodyElement(): SVGElement {
        return this.node;
    }

    getBlock(): FlowBlock {
        return this.block;
    }

    onContentUpdate(contents: FlowBlock[]) {
        // Obtain new distribution
        const uiContents = contents.filter(b => b instanceof UiFlowBlock) as UiFlowBlock[];
        // Update grid
        try {
            const tree = ResponsivePageGenerateTree(this, contents);

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
}

function performCuts(tree: CutTree, contents: UiFlowBlock[], width: number, height: number, offset: Position2D) {
    const acc = [];
    let todo = [{ tree: tree, area: { x: 0, y: 0, width, height } }];

    const blocks = {};
    for (const block of contents) {
        blocks[block.id] = block;
    }

    while (todo.length > 0) {
        const cut = todo.pop();

        if ((cut.tree as UiElementRepr).widget_type) {
            continue; // Nothing to do
        }

        const cTree = cut.tree as CutNode;
        const elements = cTree.groups.map(g => getElementsInGroup(g));

        if (cTree.cut_type === 'vertical') {
            // This cloning is probably not needed. Done now for simplicity.
            const availArea = Object.assign({}, cut.area);

            // Analyze as group pairs
            for (let i = 1; i < cTree.groups.length; i++) {

                const r1 = getRect(elements[i - 1].map(e => blocks[e]));
                const r2 = getRect(elements[i].map(e => blocks[e]));

                const cutYPos = (r1.bottom + (r2.top - r1.bottom) / 2) - offset.y;

                acc.push({ from: { x: availArea.x, y: cutYPos }, to: { x: availArea.x + availArea.width, y: cutYPos }, type: 'vert' });

                const topArea = Object.assign({}, availArea);
                topArea.height = cutYPos - availArea.y;
                todo.push({ tree: cTree.groups[i - 1], area: topArea });

                availArea.y = cutYPos;
                availArea.height -= topArea.height;

                if ((i + 1) === cTree.groups.length) {
                    // Only recurse bottom on the latest group, to avoid duplicating
                    todo.push({ tree: cTree.groups[i], area: availArea });
                }
            }
        }
        else if (cTree.cut_type === 'horizontal') {
            // This cloning is probably not needed. Done now for simplicity.
            const availArea = Object.assign({}, cut.area);

            // Analyze as group pairs
            for (let i = 1; i < cTree.groups.length; i++) {

                const r1 = getRect(elements[i - 1].map(e => blocks[e]));
                const r2 = getRect(elements[i].map(e => blocks[e]));

                const cutXPos = (r1.right + (r2.left - r1.right) / 2) - offset.x;

                acc.push({ from: { x: cutXPos, y: availArea.y }, to: { x: cutXPos, y: availArea.y + availArea.height }, type: 'horiz' });

                const leftArea = Object.assign({}, availArea);
                leftArea.width = cutXPos - availArea.x;
                todo.push({ tree: cTree.groups[i - 1], area: leftArea });

                availArea.x = cutXPos;
                availArea.width -= leftArea.width;

                if ((i + 1) === cTree.groups.length) {
                    // Only recurse right on the latest group, to avoid duplicating
                    todo.push({ tree: cTree.groups[i], area: availArea });
                }
            }
        }
        else {
            throw new Error("Unknown cut type: " + cTree.cut_type);
        }
    }

    return acc;
}

function getElementsInGroup(tree: CutTree): string[] {
    const acc = [];

    const todo = [tree];
    while (todo.length > 0) {
        const cut = todo.pop();
        if ((cut as UiElementRepr).widget_type) {
            acc.push((cut as UiElementRepr).id);
        }
        else {
            for (const group of (cut as CutNode).groups) {
                todo.push(group);
            }
        }
    }

    return acc;
}

function getRect(blocks: UiFlowBlock[]) {
    const initialArea = blocks[0].getBodyArea();
    let rect = {
        left: initialArea.x,
        top: initialArea.y,
        right: initialArea.x + initialArea.width,
        bottom: initialArea.y + initialArea.height,
    };

    for (let i = 1; i < blocks.length; i++) {
        const bArea = blocks[i].getBodyArea();

        let bRect = {
            left: bArea.x,
            top: bArea.y,
            right: bArea.x + bArea.width,
            bottom: bArea.y + bArea.height,
        };

        rect.left = Math.min(rect.left, bRect.left);
        rect.top = Math.min(rect.top, bRect.top);
        rect.right = Math.max(rect.right, bRect.right);
        rect.bottom = Math.max(rect.bottom, bRect.bottom);
    }

    return rect;
}

export const ResponsivePageGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    // Format in a grid-like
    const uiPos = (blocks
        .filter(b => b instanceof UiFlowBlock)
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
function reduceTree(tree: CutTree) {
    const aux = (node: CutTree) => {
        if (!((node as CutNode).cut_type)) {
            return node;
        }

        const cNode = node as CutNode;
        const newGroups = reduceGroups(cNode);

        return { cut_type: cNode.cut_type, groups: newGroups };
    };

    return aux(tree);
}

function reduceGroups(cNode: CutNode): CutTree[] {
    let acc = [];
    const cType = cNode.cut_type;

    const aux = (tree: CutTree) => {
        if (!(tree as CutNode).cut_type) {
            acc.push(tree);
            return;
        }

        const cTree = tree as CutNode;
        if (cTree.cut_type === cType) {
            for (const group of cTree.groups) {
                aux(group);
            }
        }
        else {
            acc.push(reduceTree(cTree));
        }
    }

    for (const group of cNode.groups) {
        aux(group);
    }

    return acc;
}

// Recursively perform cleanestCut, until all elements are partitioned.
function cleanestTree(elems: CutElement[], blocks: UiFlowBlock[]): CutTree {
    if (elems.length < 1) {
        throw new Error(`Cannot build tree with < 1 element, found ${elems.length}`);
    }
    else if (elems.length === 1) {
        const block = blocks[elems[0].i];
        return block.renderAsUiElement();
    }

    const cut = cleanestCut(elems);
    return { cut_type: cut.cutType, groups: cut.groups.filter(g => g.length > 0).map(g => cleanestTree(g, blocks)) };
}

// Perform a single cut that divides the elements on the place where there is more empty space.
function cleanestCut(elems: CutElement[]): { cutType: CutType, groups: CutElement[][] } {

    // Sort horizantally and vertically
    const horiz = elems.concat([]);
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
            horizSpaces.push([ -1, idx, e ]);
            continue;
        }

        const diff = e.a.x - endX;
        if (diff <= 0) {
            horizSpaces.push([ -1, idx, e ]);
            continue;
        }
        endX = e.a.x + e.a.width;

        horizSpaces.push([ diff, idx, e ]);
    }

    horizSpaces.sort(([a, _aIdx], [b, _bIdx]) => b - a)

    // Measure vertical spaces
    const vertSpaces = [];
    let endY = null;
    for (let idx = 0; idx < vert.length; idx++) {
        const e = vert[idx];

        if (endY === null) {
            endY = e.a.y + e.a.height;
            vertSpaces.push([ -1, idx, e ]);
            continue;
        }

        const diff = e.a.y - endY;
        if (diff <= 0) {
            vertSpaces.push([ -1, idx, e ]);
            continue;
        }
        endY = e.a.y + e.a.height;

        vertSpaces.push([ diff, idx, e ]);
    }

    vertSpaces.sort(([a, _aIdx], [b, _bIdx]) => b - a)

    // Find how to cut, horizontally or vertically
    let cutType : CutType = 'no-cut';
    if (horizSpaces.length === 0) {
        if (vertSpaces.length === 0) {
            cutType = 'no-cut';
        }
        else {
            cutType = 'vertical';
        }
    }
    else if (vertSpaces.length === 0) {
        cutType = 'horizontal';
    }
    else {
        const maxHoriz = horizSpaces[0][0];
        const maxVert = vertSpaces[0][0];

        if (maxHoriz > maxVert) {
            cutType = 'horizontal';
        }
        else {
            cutType = 'vertical';
        }
    }

    if (cutType === 'no-cut') {
        return { cutType:'no-cut', groups: [elems] };
    }

    // Perform the cut on the index with the most space
    let before: CutElement[], after:CutElement[];
    if (cutType === 'horizontal') {
        const cutIdx = horizSpaces[0][1];
        before = horiz.slice(0, cutIdx);
        after = horiz.slice(cutIdx);
    }
    else {
        const cutIdx = vertSpaces[0][1];
        before = vert.slice(0, cutIdx);
        after = vert.slice(cutIdx);
    }

    return { cutType: cutType, groups: [before, after] };
}
