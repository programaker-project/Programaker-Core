import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { BlockAllowedConfigurations, BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { Area2D, FlowBlock } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockBuilder, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { Autoresizable, TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { PositionResponsiveContents, SEPARATION } from "./positioning";
import { getElementsInGroup, getRect, ResponsivePageGenerateTree } from "./responsive_page";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { ContainerElementRepr, CutTree } from "./ui_tree_repr";
import { combinedArea, listToDict, manipulableAreaToArea2D } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const BLOCK_TYPE_ANNOTATION = 'Ui Card'
const DEFAULT_COLOR = '';

const MIN_WIDTH = 100;
const MIN_HEIGHT = 100;

export const SimpleUiCardBuilder: ContainerFlowBlockBuilder = (canvas: SVGElement,
    group: SVGElement,
    block: ContainerFlowBlock,
    service: UiSignalService,
    initOps: UiFlowBlockBuilderInitOps,
) => {
    const element = new SimpleUiCard(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class SimpleUiCard implements ContainerFlowBlockHandler, HandleableElement, Autoresizable, ConfigurableSettingsElement {
    subscription: Subscription;
    handle: UiElementHandle | null = null;
    node: SVGGElement;
    rect: SVGRectElement;
    rectShadow: SVGRectElement;
    grid: SVGGElement;
    width: number;
    height: number;
    placeholder: SVGTextElement;
    container: ContainerFlowBlock;
    private _contents: FlowBlock[] = [];

    constructor(canvas: SVGElement, group: SVGElement,
        public block: ContainerFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        this.node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');
        this.placeholder = document.createElementNS(SvgNS, 'text');

        group.setAttribute('class', 'flow_node ui_node container_node card_node simple_card');

        this.grid = document.createElementNS(SvgNS, 'g');

        this.node.appendChild(this.rectShadow);
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
        this.rect.setAttributeNS(null, 'rx', "4");

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'rx', "4");

        this.grid.setAttribute('class', 'division_grid');

        this.updateStyle();
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

        this.rectShadow.setAttributeNS(null, 'width', this.width + '');
        this.rectShadow.setAttributeNS(null, 'height', this.height + '');

        this.block.blockData.dimensions = { width: this.width, height: this.height };

        this._updateInternalElementSizes();
        if (this.handle) {
            this.handle.update();
        }
    }

    _updateInternalElementSizes() {
        this.rect.setAttributeNS(null, 'width', this.width + '');
        this.rect.setAttributeNS(null, 'height', this.height + '');

        this.rectShadow.setAttributeNS(null, 'width', this.width + '');
        this.rectShadow.setAttributeNS(null, 'height', this.height + '');

        const textBox = this.placeholder.getBBox();
        this.placeholder.setAttributeNS(null, 'x', (this.width - textBox.width) / 2 + '');
        this.placeholder.setAttributeNS(null, 'y', this.height / 2  + textBox.height / 2 + '');

        this.block.blockData.dimensions = { width: this.width, height: this.height };
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }


    // Resizeable
    resize(dim: { x?: number, y?: number, width: number; height: number; }) {
        // Check that what the minimum available size is
        const fullContents = this.block.recursiveGetAllContents();

        const inflexibleArea = combinedArea(
            fullContents
                .filter(b => b instanceof UiFlowBlock)
                .filter(b => (!(b instanceof ContainerFlowBlock)) || (b.isAutoresizable()))
                .map((b: UiFlowBlock) => {
                    const area = b.getBodyArea();

                    if (b.isAutoresizable()) {
                        const min = b.getMinSize();
                        area.width = min.width;
                        area.height = min.height;
                    }
                    return area;
                }));

        const wasPos = this.block.getOffset();

        const mov = {
            x: (inflexibleArea.x - SEPARATION) - wasPos.x,
            y: (inflexibleArea.y - SEPARATION) - wasPos.y,
        };

        (this.block as ContainerFlowBlock).moveWithoutCarrying(mov);

        const pos = this.block.getOffset();

        const minWidth = Math.max(
            MIN_WIDTH,
            inflexibleArea.width === 0 ? 0 : inflexibleArea.x - pos.x + inflexibleArea.width + SEPARATION,
        );

        const minHeight = Math.max(
            MIN_HEIGHT,
            inflexibleArea.height === 0 ? 0 : inflexibleArea.y - pos.y + inflexibleArea.height + SEPARATION,
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

    isAutoresizable(): this is Autoresizable {
        return true;
    }

    doesTakeAllHorizontal() {
        return false;
    }

    getMinSize() {
        if (this._contents.length === 0) {
            return { width: MIN_WIDTH, height: MIN_HEIGHT };
        }
        const area = this.rect.getBBox();

        return {
            width: Math.max(area.width, MIN_WIDTH),
            height: Math.max(area.height, MIN_HEIGHT),
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
    }

    repositionContents(): void {
        if (this._contents.length === 0) {
            return;
        }

        const allContents = this.block.recursiveGetAllContents();
        const cutTree = PositionResponsiveContents(this, this._contents, allContents, this.getBodyArea());

        const contentDict = listToDict(
            allContents.filter(x => x instanceof UiFlowBlock) as UiFlowBlock[],
            c => c.id);

        const elems = getElementsInGroup(cutTree)
            .map(id => contentDict[id])
            .filter(x => x.isHorizontallyStackable());

        const newArea = getRect(elems);

        this.resize(manipulableAreaToArea2D(newArea));
    }

    updateContainer(container: UiFlowBlock | null) {
        if (container instanceof ContainerFlowBlock) {
            this.container = container;
        }
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
    treeWith(content: CutTree): CutTree {
        const tree: ContainerElementRepr = {
            container_type: 'simple_card',
            id: this.block.id,
            content: content,
            settings: {},
        };

        const settings = this.block.blockData.settings;
        if (settings) {
            if (settings.bg && settings.bg.type === 'color') {
                tree.settings.bg = settings.bg;
            }
        }

        return tree;
    }
}

export const SimpleUiCardGenerateTree: GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => {
    const content = ResponsivePageGenerateTree(handler, blocks);

    // Finally generate the tree
    return (handler as SimpleUiCard).treeWith(content);
};
