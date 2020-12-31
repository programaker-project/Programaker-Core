import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { BlockAllowedConfigurations, BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { Area2D, FlowBlock, Resizeable } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockBuilder, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { CutTree, ContainerElementRepr } from "./ui_tree_repr";
import { combinedArea, getRefBox } from "./utils";
import { ResponsivePageGenerateTree } from "./responsive_page";
import { PositionResponsiveContents } from "./positioning";


const SvgNS = "http://www.w3.org/2000/svg";
const BLOCK_TYPE_ANNOTATION = 'Link Area'
const SECTION_PADDING = 5;

const MIN_WIDTH = 100;
const MIN_HEIGHT = 100;

export const LinkAreaBuilder: ContainerFlowBlockBuilder = (canvas: SVGElement,
    group: SVGElement,
    block: ContainerFlowBlock,
    service: UiSignalService,
    initOps: UiFlowBlockBuilderInitOps,
) => {
    const element = new LinkArea(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class LinkArea implements ContainerFlowBlockHandler, HandleableElement, Resizeable, ConfigurableSettingsElement {
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

    constructor(canvas: SVGElement, group: SVGElement,
        public block: ContainerFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        this.node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.placeholder = document.createElementNS(SvgNS, 'text');

        group.setAttribute('class', 'flow_node ui_node container_node card_node action_area');

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
            this.handle = new UiElementHandle(this, this.node, initOps.workspace, ['resize_width_height', 'adjust_settings']);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }


    updateSizes() {
        this.rect.setAttributeNS(null, 'width', this.width + '');
        this.rect.setAttributeNS(null, 'height', this.height + '');

        this.block.blockData.dimensions = { width: this.width, height: this.height };

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
    resize(dim: { width: number; height: number; }) {
        // Check that what the minimum available size is
        const fullContents = this.block.recursiveGetAllContents();

        const inflexibleArea = combinedArea(
            fullContents
                .filter(b => !(
                    (b instanceof ContainerFlowBlock) || ((b instanceof UiFlowBlock) && b.isAutoresizable())
                ))
                .map(b => b.getBodyArea()));

        const pos = this.block.getOffset();

        const minWidth = Math.max(
            MIN_WIDTH,
            inflexibleArea.x - pos.x + inflexibleArea.width,
        );

        const minHeight = Math.max(
            MIN_HEIGHT,
            inflexibleArea.y - pos.y + inflexibleArea.height,
        );

        this.width = Math.max(minWidth, dim.width);
        this.height = Math.max(minHeight, dim.height);

        this.updateSizes();

        (this.block as ContainerFlowBlock).update();
        this.handle.update();

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
        // TODO: Merge this with SimpleUiCard.onContentUpdate()?
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
        // Push horizontally
        const aggregatedWidth = containedArea.width + (containedArea.x - oldPos.x);
        if (this.width < aggregatedWidth) {
            updated = true;
            const oldWidth = this.width;

            this.width = aggregatedWidth;

            // Push elements down
            const pushRight = this.width - oldWidth;
            if (this.container && pushRight > 0) {
                this.container.pushRight(oldPos.x + oldWidth, pushRight);
            }
        }
        // Push vertically
        const aggregatedHeight = containedArea.height + (containedArea.y - oldPos.y);
        if (this.height < containedArea.height) {
            updated = true;
            const oldHeight = this.height;

            this.height = aggregatedHeight;

            // Push elements down
            const pushDown = this.height - oldHeight;
            if (this.container && pushDown > 0) {
                this.container.pushDown(oldPos.y + oldHeight, pushDown);
            }

        }

        if (updated) {
            this.updateSizes();
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
        }
    }

    repositionContents(): void {
        PositionResponsiveContents(this, this._contents, this.block.recursiveGetAllContents(), this.getBodyArea());
    }

    dropOnEndMove() {
        return { x: 0, y: 0 };
    }

    update() {
        this.onContentUpdate(this._contents);
    }

    // Configurable
    startAdjustingSettings(): void {
        this.block.workspace.startBlockConfiguration(this);
    }

    getAllowedConfigurations(): BlockAllowedConfigurations {
        return { target: { link: true } };
    }

    getCurrentConfiguration(): BlockConfigurationOptions {
        return Object.assign({}, this.block.blockData.settings || {});
    }

    applyConfiguration(settings: BlockConfigurationOptions): void {
        const settingsStorage = Object.assign({}, this.block.blockData.settings || {});

        if (settings.target && settings.target.link) {
            if (!settingsStorage.target) {
                settingsStorage.target = {};
            }
            settingsStorage.target.link = { value: settings.target.link.value };
        }

        this.block.blockData.settings = settingsStorage;
    }

    // Compilation
    treeWith(content: CutTree): CutTree {
        const tree: ContainerElementRepr = {
            container_type: 'link_area',
            id: this.block.id,
            content: content,
            settings: this.block.blockData.settings || {},
        };

        const settings = this.block.blockData.settings;
        if (settings) {
            tree.settings = settings;
        }

        return tree;
    }
}

export const LinkAreaGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    const content = ResponsivePageGenerateTree(handler, blocks);

    // Finally generate the tree
    return (handler as LinkArea).treeWith(content);
};
