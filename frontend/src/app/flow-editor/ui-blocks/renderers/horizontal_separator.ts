import { UiSignalService } from "../../../services/ui-signal.service";
import { BlockAllowedConfigurations, BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { Area2D, FlowBlock } from "../../flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockBuilderInitOps, UiFlowBlockHandler, Autoresizable } from "../ui_flow_block";
import { UiElementHandle, ConfigurableSettingsElement } from "./ui_element_handle";
import { ContainerFlowBlock } from "../container_flow_block";


const SvgNS = "http://www.w3.org/2000/svg";

const Label = "- Horizontal separator -";

export const HorizontalSeparatorBuilder: UiFlowBlockBuilder = (canvas: SVGElement,
    group: SVGElement,
    block: UiFlowBlock,
    service: UiSignalService,
    initOps: UiFlowBlockBuilderInitOps,
) => {
    const element = new HorizontalSeparator(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class HorizontalSeparator implements UiFlowBlockHandler, Autoresizable, ConfigurableSettingsElement {
    private textBox: SVGTextElement;
    private rect: SVGRectElement;
    readonly MinWidth = 120;
    private handle: UiElementHandle;
    private _container: ContainerFlowBlock;
    private separatorPath: SVGPathElement;
    private _minSize: { width: number, height: number };

    constructor(canvas: SVGElement, group: SVGElement,
        private block: UiFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        const node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        const contentsGroup = document.createElementNS(SvgNS, 'g');
        this.separatorPath = document.createElementNS(SvgNS, 'path');

        group.setAttribute('class', 'flow_node ui_node separator_node');

        this.textBox = document.createElementNS(SvgNS, 'text');
        this.textBox.setAttribute('class', 'text');
        this.textBox.setAttributeNS(null, 'textlength', '100%');

        this.textBox.textContent = Label;

        this.separatorPath.setAttribute('class', 'representation');

        contentsGroup.appendChild(this.textBox);
        node.appendChild(this.rect);
        node.appendChild(this.separatorPath);
        node.appendChild(this.textBox);
        group.appendChild(node);

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.separatorPath.setAttributeNS(null, 'x', "0");
        this.separatorPath.setAttributeNS(null, 'y', "0");

        this.updateStyle();
        this._updateSize();

        this._minSize = this.rect.getBBox();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, node, initOps.workspace, ['adjust_settings']);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }

    getArea(): Area2D {
        return this.rect.getBBox();
    }

    isTextEditable(): this is TextEditable {
        return false;
    }

    isTextReadable(): this is TextReadable {
        return false;
    }

    get isStaticText(): boolean {
        return false;
    }

    dispose() {}

    onInputUpdated(block: FlowBlock, inputIndex: number) {}

    onConnectionValueUpdate(inputIndex: number, value: string) {}

    onConnectionLost(portIndex: number) {}

    // Focus management
    onClick() {}

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

    // Resizing
    readonly isNotHorizontallyStackable = true;
    isAutoresizable(): this is Autoresizable {
        return true;
    }

    getMinSize() {
        return {
            width: this._minSize.width,
            height: this._minSize.height,
        }
    }

    // Handleable element
    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    getBodyElement(): SVGRectElement {
        return this.rect;
    }

    getBlock(): FlowBlock {
        return this.block;
    }

    // Configurable element
    startAdjustingSettings(): void {
        this.block.workspace.startBlockConfiguration(this);
    }

    applyConfiguration(settings: BlockConfigurationOptions): void {
        const settingsStorage = Object.assign({}, this.block.blockData.settings || {});

        if (!settingsStorage.body) {
            settingsStorage.body = {};
        }

        if (settings.body) {
            if (settings.body.widthTaken) {
                settingsStorage.body.widthTaken = { value: settings.body.widthTaken.value };
            }
        }

        this.block.blockData.settings = settingsStorage;
        this.updateStyle();
        this._updateSize(); // Style changes might change the block's size

        if (this.handle) {
            this.handle.update();
        }
    }

    getCurrentConfiguration(): BlockConfigurationOptions {
        const config = Object.assign({}, this.block.blockData.settings || {});

        if (!config.body) {
            config.body = {};
        }
        if (!config.body.widthTaken) {
            config.body.widthTaken = {value: 'half'};
        }

        return config;
    }

    getAllowedConfigurations(): BlockAllowedConfigurations {
        return {
            body: {
                widthTaken: [
                    { name: 'short', style: '20%' },
                    { name: 'half', style: '50%' },
                    { name: 'full', style: '100%' },
                ],
            },
        };
    }

    // When inside a container, push to the right and cover all width
    updateContainer(container: UiFlowBlock | null) {
        if (container instanceof ContainerFlowBlock) {
            this._container = container;
        }
        else {
            this._container = null;
        }
        this._updateSize();
    }

    dropOnEndMove() {
        if (!this._container) {
            return {x: 0, y: 0};
        }

        let result = {x: 0, y: 0};

        const parentArea = this._container.getBodyArea();
        const offset = this.block.getOffset();
        const xdiff = offset.x - (parentArea.x + 1);
        if (xdiff != 2) {
            result = { x: -xdiff, y: 0};
        }

        this._updateSize();
        return result;
    }

    // Style management
    updateStyle() {
        const settings = this.block.blockData.settings;
        if (!settings) {
            return;
        }

        // Nothing to do here, actually...
    }

    // Aux
    _updateSize() {
        const textArea = this.textBox.getClientRects()[0];

        const box_height = textArea.height * 1.5;
        let box_width = Math.max(textArea.width + 50, this.MinWidth);

        if (this._container) {
            const parentArea = this._container.getBodyArea();
            box_width = parentArea.width - 2;
        }

        this.textBox.setAttributeNS(null, 'y', box_height/2 + "");
        this.textBox.setAttributeNS(null, 'x', (box_width - textArea.width)/2 + "");

        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");


        this.separatorPath.setAttributeNS(null, 'height', box_height + "");
        this.separatorPath.setAttributeNS(null, 'width', box_width + "");

        const settings = this.block.blockData.settings;
        let widthTaken = 'half';
        if (settings && settings.body && settings.body.widthTaken) {
            widthTaken = settings.body.widthTaken.value;
        }

        if (widthTaken == 'short') {
            this.separatorPath.setAttributeNS(null, 'd', `M${(box_width / 10) * 4},${box_height / 1.25} H${(box_width / 10) * 6}`);
        }
        else if (widthTaken === 'full') {
            this.separatorPath.setAttributeNS(null, 'd', `M0,${box_height / 1.25} H${box_width}`);
        }
        else {
            this.separatorPath.setAttributeNS(null, 'd', `M${box_width / 4},${box_height / 1.25} H${(box_width / 4) * 3}`);
        }

        if (this.handle) {
            this.handle.update();
        }
    }
}
