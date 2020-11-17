import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock, Resizeable } from "../../flow_block";
import { ContainerFlowBlock, GenTreeProc } from "../container_flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { ContainerElement, ContainerElementHandle } from "./container_element_handle";
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

class ResponsivePage implements UiFlowBlockHandler, ContainerElement, Resizeable {
    subscription: Subscription;
    text: SVGTextElement;
    handle: ContainerElementHandle | null = null;
    node: SVGAElement;
    textDim: { width: number; height: number; };
    rect: SVGRectElement;
    rectShadow: SVGRectElement;

    constructor(canvas: SVGElement, group: SVGElement,
                private block: UiFlowBlock,
                private service: UiSignalService,
                private initOps: UiFlowBlockBuilderInitOps) {

        const refBox = getRefBox(canvas);

        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node container_node responsive_page');

        this.text = document.createElementNS(SvgNS, 'text');
        this.text.setAttribute('class', 'output_text');
        this.text.setAttributeNS(null,'textlength', '100%');

        this.text.textContent = Title;

        this.node.appendChild(this.rectShadow);
        this.node.appendChild(this.rect);
        this.node.appendChild(this.text);
        group.appendChild(this.node);

        const text_width = this.text.getClientRects()[0].width;
        const text_height = this.text.getClientRects()[0].height;
        this.textDim = { width: text_width, height: text_height };


        let box_height = refBox.height * 10;
        let box_width = text_width * 1.25;
        if (block.blockData.dimensions) {
            box_height = block.blockData.dimensions.height;
            box_width = block.blockData.dimensions.width;
        }

        this.text.setAttributeNS(null, 'y', box_height/2 - text_height / 2 + "");
        this.text.setAttributeNS(null, 'x', (box_width - text_width)/2 + "");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");


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
        const width = Math.max(this.textDim.width, dim.width);
        const height = Math.max(this.textDim.height, dim.height);

        this.rect.setAttributeNS(null, 'width', width + '');
        this.rect.setAttributeNS(null, 'height', height + '');

        this.rectShadow.setAttributeNS(null, 'width', width + '');
        this.rectShadow.setAttributeNS(null, 'height', height + '');

        this.text.setAttributeNS(null, 'x', (width - this.textDim.width)/2 + "");
        this.text.setAttributeNS(null, 'y', (height/2 + this.textDim.height/2) + "");

        this.block.blockData.dimensions = { width, height };

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

}

type CutElement = {i: number, a: Area2D};
type CutType = 'vertical' | 'horizontal' | 'no-cut';
type CutTree = CutElement | { cutType:CutType, groups: CutTree[] };

export const ResponsivePageGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    // Format in a grid-like
    const pos = blocks.map((b, i) => {
        return {i, a: b.getBodyArea()};
    });

    if (blocks.length < 2) {
        return blocks.map(b => b.getBodyArea());
    }

    const tree = cleanestTree(pos);

    // TODO: Complete

    return tree;
}

function cleanestTree(elems: CutElement[]): CutTree {
    if (elems.length < 1) {
        throw new Error(`Cannot build tree with < 1 element, found ${elems.length}`);
    }
    else if (elems.length === 1) {
        return elems[0];
    }

    const cut = cleanestCut(elems);
    return { cutType: cut.cutType, groups: cut.groups.map(g => cleanestTree(g)) };
}

function cleanestCut(elems: CutElement[]): { cutType:CutType, groups: CutElement[][] } {

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
