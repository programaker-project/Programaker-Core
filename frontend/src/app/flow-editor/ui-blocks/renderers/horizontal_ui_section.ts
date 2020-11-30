import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock, Resizeable } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockBuilder, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { ContainerElement, ContainerElementHandle } from "./container_element_handle";
import { combinedArea, getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const BLOCK_TYPE_ANNOTATION = 'Horizontal section'
const SECTION_PADDING = 5;

export const HorizontalUiSectionBuilder : ContainerFlowBlockBuilder = (canvas: SVGElement,
                                                                  group: SVGElement,
                                                                  block: ContainerFlowBlock,
                                                                  service: UiSignalService,
                                                                  initOps: UiFlowBlockBuilderInitOps,
                                                                 ) => {
    const element = new HorizontalUiSection(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class HorizontalUiSection implements ContainerFlowBlockHandler, ContainerElement, Resizeable {
    subscription: Subscription;
    handle: ContainerElementHandle | null = null;
    node: SVGAElement;
    rect: SVGRectElement;
    grid: SVGGElement;
    width: number;
    height: number;
    placeholder: SVGTextElement;
    container: ContainerFlowBlock;
    private _contents: FlowBlock[];

    constructor(canvas: SVGElement, group: SVGElement,
                public block: ContainerFlowBlock,
                private service: UiSignalService,
                private initOps: UiFlowBlockBuilderInitOps) {

        const refBox = getRefBox(canvas);

        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.placeholder = document.createElementNS(SvgNS, 'text');

        group.setAttribute('class', 'flow_node ui_node container_node responsive_page');

        this.grid = document.createElementNS(SvgNS, 'g');

        this.node.appendChild(this.rect);
        this.node.appendChild(this.grid);
        this.node.appendChild(this.placeholder);

        group.appendChild(this.node);


        this.placeholder.setAttributeNS(null, 'class', 'block_type_annotation');
        this.placeholder.textContent = BLOCK_TYPE_ANNOTATION;

        const text_width = this.placeholder.getClientRects()[0].width;
        const text_height = this.placeholder.getClientRects()[0].height;
        const textDim = { width: text_width, height: text_height };

        const bdims = block.blockData.dimensions;
        this.width = bdims ? bdims.width : textDim.width * 1.5;
        this.height = bdims ? bdims.height : textDim.height * 2;

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.grid.setAttribute('class', 'division_grid');

        this.updateSizes();

        if (initOps.workspace) {
            this.handle = new ContainerElementHandle(this, initOps.workspace, [ 'resize_height' ]);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }

    updateSizes() {
        if (this.container) {
            const area = this.container.getBodyArea();
            this.width = area.width - 2;

            const offset = this.block.getOffset();
            const xdiff = offset.x - (area.x + 1);
            if (xdiff != 2) {
                this.block.moveBy({ x: -xdiff, y: 0});
            }
        }

        this._updateInternalElementSizes();

        if (this.handle) {
            this.handle.update();
        }
    }

    _updateInternalElementSizes() {
        this.rect.setAttributeNS(null, 'width', this.width + '');
        this.rect.setAttributeNS(null, 'height', this.height + '');

        const textBox = this.placeholder.getBBox();
        this.placeholder.setAttributeNS(null, 'x', (this.width - textBox.width) / 2 + '');
        this.placeholder.setAttributeNS(null, 'y', this.height / 2  + textBox.height / 2 + '');

        this.block.blockData.dimensions = { width: this.width, height: this.height };
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    // Resizeable
    resize(dimensions: { width: number; height: number; }) {
        // Make sure what's the minimum possible height
        const fullContents = this.block.recursiveGetAllContents();

        const pos = this.block.getOffset();
        let minHeight = 0;

        if (fullContents.length > 0) {

            const inflexibleArea = combinedArea(
                fullContents
                    .filter(b => !(b instanceof ContainerFlowBlock))
                    .map(b => b.getBodyArea()));

            minHeight = inflexibleArea.y - pos.y + inflexibleArea.height;
        }

        const oldHeight = this.height;
        const newHeight = Math.max(minHeight, dimensions.height);

        this.height = newHeight;
        this.updateSizes();

        const pushDown = newHeight - oldHeight;
        if (this.container && pushDown > 0) {
            this.container.pushDown(pos.y + oldHeight, pushDown);
        }
    }

    // UiFlowBlock
    onClick() {
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
        this._contents = contents;

        // Check that the container size is adequate, resize it if necessary
        const fullContents = this.block.recursiveGetAllContents();

        if (fullContents.length === 0) {
            return;
        }

        // Move contents down if they are not (vertically) completely inside the section
        // This is done one-by-one, so the order of addition of blocks to the contained does not affect the final state.
        const current = this.block.getOffset();

        for (const block of contents) {
            const blockArea = block.getBodyArea();
            const yDiff = (current.y + SECTION_PADDING) - blockArea.y;
            if (yDiff > 0) {
                block.moveBy({ x: 0, y: yDiff });
            }
        }

        const containedArea = combinedArea(fullContents.map(b => b.getBodyArea()));

        // Extend the section to include all elements, if needed
        if (this.height < containedArea.height) {
            this.height = containedArea.height;
            this.updateSizes();
        }
    }

    updateContainer(container: UiFlowBlock) {
        if (container instanceof ContainerFlowBlock) {
            this.container = container;
        }
        else {
            this.container = null;
        }
        this.updateSizes();
    }

    dropOnEndMove() {
        if (!this.container) {
            return {x: 0, y: 0};
        }

        const area = this.container.getBodyArea();
        this.width = area.width - 2;

        const offset = this.block.getOffset();
        const xdiff = offset.x - (area.x + 1);
        if (xdiff != 2) {
            return { x: -xdiff, y: 0};
        }

        this._updateInternalElementSizes();
    }

    update() {
        this.onContentUpdate(this._contents);
    }
}

export const HorizontalUiSectionGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    return { cut_type: 'hbox', groups: blocks.filter(b => b instanceof UiFlowBlock).map((b: UiFlowBlock) => b.renderAsUiElement()) };
}
