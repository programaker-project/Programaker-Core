import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { BlockAllowedConfigurations, BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { Area2D, FlowBlock, Resizeable } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockBuilder, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { CutTree } from "./ui_tree_repr";
import { combinedArea, getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const BLOCK_TYPE_ANNOTATION = 'Horizontal section'
const SECTION_PADDING = 5;
const DEFAULT_COLOR = '';

export const HorizontalUiSectionBuilder: ContainerFlowBlockBuilder = (canvas: SVGElement,
    group: SVGElement,
    block: ContainerFlowBlock,
    service: UiSignalService,
    initOps: UiFlowBlockBuilderInitOps,
) => {
    const element = new HorizontalUiSection(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class HorizontalUiSection implements ContainerFlowBlockHandler, HandleableElement, Resizeable, ConfigurableSettingsElement {
    subscription: Subscription;
    handle: UiElementHandle | null = null;
    node: SVGGElement;
    rect: SVGRectElement;
    grid: SVGGElement;
    width: number;
    height: number;
    placeholder: SVGTextElement;
    container: ContainerFlowBlock;
    private _contents: FlowBlock[];
    nestedHorizontal: boolean;

    constructor(canvas: SVGElement, group: SVGElement,
        public block: ContainerFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        this.node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.placeholder = document.createElementNS(SvgNS, 'text');

        group.setAttribute('class', 'flow_node ui_node container_node section_node');

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

        this.updateStyle();
        this.updateSizes();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, this.node, initOps.workspace, ['resize_height', 'adjust_settings']);
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
            if (this.nestedHorizontal) {
                // If the parent is an horizontal element, cover all height
                this.height = area.height - 2;

                const offset = this.block.getOffset();
                const ydiff = offset.y - (area.y + 1);
                if (ydiff != 2) {
                    this.block.moveBy({ x: 0, y: -ydiff});
                }
            }
            else {
                this.width = area.width - 2;

                const offset = this.block.getOffset();
                const xdiff = offset.x - (area.x + 1);
                if (xdiff != 2) {
                    this.block.moveBy({ x: -xdiff, y: 0});
                }
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

        if (this.nestedHorizontal) {
            // Resize vertically
            let minWidth = 0;

            if (fullContents.length > 0) {

                const inflexibleArea = combinedArea(
                    fullContents
                        .filter(b => !(
                            (b instanceof ContainerFlowBlock) || ((b instanceof UiFlowBlock) && b.isAutoresizable())
                        ))
                        .map(b => b.getBodyArea()));

                minWidth = inflexibleArea.x - pos.x + inflexibleArea.width;
            }

            const oldWidth = this.width;
            const newWidth = Math.max(minWidth, dimensions.width);

            this.width = newWidth;
            this.updateSizes();

            const pushRight = newWidth - oldWidth;
            if (this.container && pushRight > 0) {
                this.container.pushRight(pos.x + oldWidth, pushRight);
            }
        }
        else {
            // Resize horizontally
            let minHeight = 0;

            if (fullContents.length > 0) {

                const inflexibleArea = combinedArea(
                    fullContents
                        .filter(b => !(
                            (b instanceof ContainerFlowBlock) || ((b instanceof UiFlowBlock) && b.isAutoresizable())
                        ))
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

        (this.block as ContainerFlowBlock).update();

        for (const content of this._contents) {
            if (content instanceof ContainerFlowBlock) {
                content.updateContainer(this.block);
            }
            else if ((content instanceof UiFlowBlock) && content.isAutoresizable()) {
                content.updateContainer(this.block);
            }
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
    getBodyElement(): SVGRectElement {
        return this.rect;
    }

    getBlock(): FlowBlock {
        return this.block;
    }

    onContentUpdate(contents: FlowBlock[]) {
        this._contents = contents;

        // Check that the container size is adequate, resize it if necessary
        const uiContents = (this.block
            .recursiveGetAllContents()
            .filter(b => !(
                (b instanceof ContainerFlowBlock) || ((b instanceof UiFlowBlock) && b.isAutoresizable())
            )));

        if (uiContents.length === 0) {
            return;
        }

        // Move contents down if they are not (vertically) completely inside the section
        // This is done one-by-one, so the order of addition of blocks to the contained does not affect the final state.
        const current = this.block.getOffset();

        for (const block of contents) {
            const blockArea = block.getBodyArea();

            const xDiff = (current.x + SECTION_PADDING) - blockArea.x;
            const yDiff = (current.y + SECTION_PADDING) - blockArea.y;
            if ((yDiff > 0) || (xDiff > 0)) {
                block.moveBy({ x: Math.max(0, xDiff), y: Math.max(0, yDiff) });
            }
        }

        const containedArea = combinedArea(uiContents.map(b => b.getBodyArea()));
        const oldPos = this.block.getOffset();
        let updated = false;

        // Extend the section to include all elements, if needed
        if (this.nestedHorizontal) {
            // Can push horizontally
            const aggregatedWidth = containedArea.width + (containedArea.x - oldPos.x);
            if (this.width < aggregatedWidth) {
                updated = true;
                const oldWidth = this.width;

                this.width = aggregatedWidth;
                this.updateSizes();

                // Push elements down
                const pushRight = this.width - oldWidth;
                if (this.container && pushRight > 0) {
                    this.container.pushRight(oldPos.x + oldWidth, pushRight);
                }

            }
        }
        else {
            // Can push vertically
            const aggregatedHeight = containedArea.height + (containedArea.y - oldPos.y);
            if (this.height < containedArea.height) {
                updated = true;
                const oldHeight = this.height;

                this.height = aggregatedHeight;
                this.updateSizes();

                // Push elements down
                const pushDown = this.height - oldHeight;
                if (this.container && pushDown > 0) {
                    this.container.pushDown(oldPos.y + oldHeight, pushDown);
                }

            }
        }

        if (updated) {
            for (const content of this._contents) {
                if (content instanceof ContainerFlowBlock) {
                    content.updateContainer(this.block);
                }
            }
        }
    }

    updateContainer(container: UiFlowBlock | null) {
        if (container instanceof ContainerFlowBlock) {
            this.container = container;
            this.nestedHorizontal = (this.container.handler instanceof HorizontalUiSection) && (!this.container.handler.nestedHorizontal);
        }
        else {
            this.container = null;

            this.nestedHorizontal = false;
        }
        if (this.nestedHorizontal) {
            this.handle.setResizeType('horizontal');
        }
        else {
            this.handle.setResizeType('vertical');
        }
        this.updateSizes();
    }

    dropOnEndMove() {
        if (!this.container) {
            return {x: 0, y: 0};
        }

        let result = {x: 0, y: 0};

        const area = this.container.getBodyArea();
        if (this.nestedHorizontal) {
            // If the parent is an horizontal element, cover all height
            this.height = area.height - 2;

            const offset = this.block.getOffset();
            const ydiff = offset.y - (area.y + 1);
            if (ydiff != 2) {
                result = { x: 0, y: -ydiff};
            }
        }
        else {
           this.width = area.width - 2;

            const offset = this.block.getOffset();
            const xdiff = offset.x - (area.x + 1);
            if (xdiff != 2) {
                result = { x: -xdiff, y: 0};
            }
        }

        this._updateInternalElementSizes();
        return result;
    }

    update() {
        this.onContentUpdate(this._contents);
    }

    // Configurable
    startAdjustingSettings(): void {
        this.block.workspace.startBlockConfiguration(this);
    }

    getAllowedConfigurations(): BlockAllowedConfigurations {
        return { background: {color: true, image: true} };
    }

    getCurrentConfiguration(): BlockConfigurationOptions {
        return Object.assign({}, this.block.blockData.settings || {});
    }

    applyConfiguration(settings: BlockConfigurationOptions): void {
        if (settings.bg) {
            this.block.blockData.settings = Object.assign(this.block.blockData.settings || {}, {bg: settings.bg});

            this.updateStyle();
        }
    }

    // Style management
    updateStyle(){
        const settings = this.block.blockData.settings;
        if (!settings) {
            return;
        }
        if (settings.bg) {
            // Get color to apply
            let color = DEFAULT_COLOR;
            if (settings.bg.type === 'color') {
                color = settings.bg.value;
            }

            // Apply it to the element's background
            this.rect.style.fill = color;
        }
    }

    // Compilation
    generateTreeWithGroups(groups: CutTree[]): CutTree {
        const tree: CutTree = { cut_type: 'hbox', groups: groups };

        if (this.nestedHorizontal) {
            // Then this works more like a VBox
            tree.cut_type = 'vbox';
        }

        const settings = this.block.blockData.settings;
        if (settings) {
            if (settings.bg && settings.bg.type === 'color') {
                tree.background = settings.bg;
            }
        }

        return tree;
    }
}

export const HorizontalUiSectionGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    const horizHandler = handler as HorizontalUiSection;
    const filterGroups = (blocks
                    // Get UI blocks
        .filter(b => b instanceof UiFlowBlock)
        .map(b => { return { area: b.getBodyArea(), block: b } }));

    if (horizHandler.nestedHorizontal) {
        // Order by from top to bottom
        filterGroups.sort((a, b) => a.area.y - b.area.y);
    }
    else {
        // Order by from left to right
        filterGroups.sort((a, b) => a.area.x - b.area.x);
    }

    // Render the elements themselves
    const groups = filterGroups.map((item: {area: Area2D, block: UiFlowBlock}) => item.block.renderAsUiElement());

    // Finally generate the tree
    return horizHandler.generateTreeWithGroups(groups);
}
