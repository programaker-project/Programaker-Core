import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { BlockAllowedConfigurations, BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { Area2D, FlowBlock } from "../../flow_block";
import { ContainerFlowBlock, ContainerFlowBlockBuilder, ContainerFlowBlockHandler, GenTreeProc } from "../container_flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockHandler, Autoresizable } from "../ui_flow_block";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { CutTree } from "./ui_tree_repr";
import { combinedArea } from "./utils";
import { PositionHorizontalContents, PositionVerticalContents, SEPARATION, GetMinSizeHorizontal, GetMinSizeVertical } from "./positioning";
import { CannotSetAsContentsError } from "../cannot_set_as_contents_error";

const SvgNS = "http://www.w3.org/2000/svg";
const BLOCK_TYPE_ANNOTATION = 'Section'
const DEFAULT_COLOR = '';

const MIN_HEIGHT = SEPARATION;
const MIN_WIDTH = SEPARATION;

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

class HorizontalUiSection implements ContainerFlowBlockHandler, HandleableElement, Autoresizable, ConfigurableSettingsElement {
    subscription: Subscription;
    handle: UiElementHandle | null = null;
    node: SVGGElement;
    rect: SVGRectElement;
    grid: SVGGElement;
    width: number;
    height: number;
    placeholder: SVGTextElement;
    container: ContainerFlowBlock;
    private _contents: FlowBlock[] = [];
    nestedHorizontal: boolean;
    private readonly freeWidth: number;
    private readonly freeHeight: number;

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

        const text_width = this.placeholder.getBoundingClientRect().width;
        const text_height = this.placeholder.getBoundingClientRect().height;
        const textDim = { width: text_width, height: text_height };

        const bdims = block.blockData.dimensions;

        const minWidth = textDim.width * 1.5;
        const minHeight = textDim.height * 2;
        this.freeWidth = this.width = bdims && bdims.width > minWidth ? bdims.width : minWidth;
        this.freeHeight = this.height = bdims && bdims.height > minHeight ? bdims.height : minHeight;

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.grid.setAttribute('class', 'division_grid');

        this.updateStyle();
        this._updateInternalElementSizes();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, this.node, initOps.workspace, ['adjust_settings']);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
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

    // Resizing
    doesTakeAllHorizontal() {
        return !this.nestedHorizontal;
    }

    doesTakeAllVertical() {
        return this.nestedHorizontal;
    }

    isAutoresizable(): this is Autoresizable {
        return true;
    }

    getMinSize() {
        if (this._contents.length === 0) {
            return { width: MIN_WIDTH, height: MIN_HEIGHT };
        }

        if (this.nestedHorizontal) {
            return GetMinSizeVertical(this._contents);
        }
        return GetMinSizeHorizontal(this._contents);
    }

    get isNotHorizontallyStackable() {
        return this.nestedHorizontal === false;
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    // Resizeable
    resize(dimensions: { width: number; height: number; }, repositioning?: boolean) {
        // Make sure what's the minimum possible height
        const fullContents = this.block.recursiveGetAllContents();

        const pos = this.block.getOffset();

        if (repositioning) {
            this.width = dimensions.width;
            this.height = dimensions.height;
        }

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

                minWidth = inflexibleArea.width;
            }

            const newWidth = Math.max(MIN_WIDTH, minWidth, dimensions.width);

            this.width = newWidth;
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

                minHeight = inflexibleArea.height;
            }

            const newHeight = Math.max(MIN_HEIGHT, minHeight, dimensions.height);

            this.height = newHeight;
        }

        this._updateInternalElementSizes();
        this.block.update();


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
        // Reject elements that cannot be horizontally stacked
        if (!this.nestedHorizontal) {
            const problematic = contents.filter((e) => {
                if (e instanceof ContainerFlowBlock && e.handler instanceof HorizontalUiSection) {
                    return false; // These can always be nested
                }
                return (e instanceof UiFlowBlock) && (!e.isHorizontallyStackable())
            });

            if (problematic.length > 0) {
                throw new CannotSetAsContentsError("Elements that cannot be horizontally stacked cannot be added to a HorizontalUiSection", problematic);
            }
        }

        this._contents = contents;
    }

    updateContainer(container: UiFlowBlock | null) {
        if (container instanceof ContainerFlowBlock) {
            this.container = container;
            this.nestedHorizontal = (this.container.handler instanceof HorizontalUiSection) && (!this.container.handler.nestedHorizontal);
        }
        else {
            this.container = null;

            this.nestedHorizontal = null;
            this.width = this.freeWidth;
            this.height = this.freeHeight;
        }
        this._updateInternalElementSizes();
    }

    repositionContents(): void {
        if (this._contents.length === 0) {
            return;
        }

        const dimensions = this._repositionContents();

        this.resize(dimensions, true);
    }

    _repositionContents() : { width: number, height: number } {
        this.stickToContainer();

        if (this.nestedHorizontal) {
            return PositionVerticalContents(this, this._contents, this.getBodyArea());
        }
        else {
            return PositionHorizontalContents(this, this._contents, this.getBodyArea());
        }
    }

    stickToContainer(){
        if (!this.container) {
            return;
        }

        const offset = this.block.getOffset();
        const area = this.container.getBodyArea();
        if (this.nestedHorizontal) {
            const ydiff = offset.y - area.y;

            if (ydiff != 0) {
                this.block.moveBy({ x: 0, y: -ydiff});
            }
        }
        else {
            const xdiff = offset.x - area.x;

            if (xdiff != 0) {
                this.block.moveBy({ x: -xdiff, y: 0});
            }
        }
    }

    dropOnEndMove() {
        if (!this.container) {
            return {x: 0, y: 0};
        }

        let result = {x: 0, y: 0};

        const offset = this.block.getOffset();
        const area = this.container.getBodyArea();
        if (this.nestedHorizontal) {
            // If the parent is an horizontal element, cover all height
            this.height = area.height;

            const ydiff = offset.y - area.y;
            if (ydiff) {
                result = { x: 0, y: -ydiff};
            }
        }
        else {
           this.width = area.width;

            const xdiff = offset.x - area.x;
            if (xdiff) {
                result = { x: -xdiff, y: 0};
            }
        }

        this._updateInternalElementSizes();

        if (this._contents.length > 0) {
            this._repositionContents();

            // If we're repositioning, there's not much to do additionally
            result = { x: 0, y: 0 };
        }

        return result;
    }

    update() {
        this.onContentUpdate(this._contents);
    }

    updateOptions() {
        this._applyConfiguration(this.block.blockData.settings || {});

        if (this.block.blockData.dimensions) {
            this.height = this.block.blockData.dimensions.height;
            this.width = this.block.blockData.dimensions.width;
            this._updateInternalElementSizes();
        }
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

    _applyConfiguration(settings: BlockConfigurationOptions): void {
        if (settings.bg) {
            this.block.blockData.settings = Object.assign(this.block.blockData.settings || {}, {bg: settings.bg});

            this.updateStyle();
        }

    }

    applyConfiguration(settings: BlockConfigurationOptions): void {
        this._applyConfiguration(settings);

        this.block.notifyOptionsChange();
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
        const tree: CutTree = { cut_type: 'hbox', groups: groups, settings: {}, block_id: this.block.id };

        if (this.nestedHorizontal) {
            // Then this works more like a VBox
            tree.cut_type = 'vbox';
        }

        const settings = this.block.blockData.settings;
        if (settings) {
            if (settings.bg && settings.bg.type === 'color') {
                tree.settings.bg = settings.bg;
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
