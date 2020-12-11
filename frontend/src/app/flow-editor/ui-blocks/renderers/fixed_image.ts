import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock, Resizeable } from "../../flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { BlockConfigurationOptions, BlockAllowedConfigurations } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";

const SvgNS = "http://www.w3.org/2000/svg";

const DefaultImageUrl = "/assets/logo-dark.png";

export const FixedImageBuilder: UiFlowBlockBuilder = (canvas: SVGElement,
    group: SVGElement,
    block: UiFlowBlock,
    service: UiSignalService,
    initOps: UiFlowBlockBuilderInitOps,
) => {
    const element = new FixedImage(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class FixedImage implements UiFlowBlockHandler, ConfigurableSettingsElement, HandleableElement, Resizeable {
    private imageBox: SVGImageElement;
    private rect: SVGRectElement;
    private handle: UiElementHandle;
    private width: number;
    private height: number;

    private readonly minWidth = 100;
    private readonly minHeight = 100;

    constructor(canvas: SVGElement, group: SVGElement,
        private block: UiFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        const node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        const contentsGroup = document.createElementNS(SvgNS, 'g');

        group.setAttribute('class', 'flow_node ui_node image_node');

        this.imageBox = document.createElementNS(SvgNS, 'image');
        this.imageBox.setAttribute('class', 'image');


        const settings = block.blockData.settings;
        let imageUrl = DefaultImageUrl;
        if (settings && settings.body && settings.body.image) {
            imageUrl = this.block.workspace.getAssetUrlOnProgram(block.blockData.settings.body.image.id);
        }
        this.imageBox.setAttributeNS(null, 'href',  imageUrl);

        this.imageBox.setAttributeNS(null, 'width', this.minWidth + '');
        this.imageBox.setAttributeNS(null, 'height', this.minHeight + '');

        contentsGroup.appendChild(this.imageBox);
        node.appendChild(this.rect);
        node.appendChild(this.imageBox);
        group.appendChild(node);

        if (this.block.blockData.dimensions) {
            this.height = this.block.blockData.dimensions.height;
            this.width = this.block.blockData.dimensions.width;
        }
        else {
            this.height = this.minHeight;
            this.width = this.minWidth;
        }

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.updateStyle();
        this._updateSize();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, node, initOps.workspace, ['adjust_settings', 'resize_width_height']);
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

    onConnectionLost(portIndex: number) {
        this.onConnectionValueUpdate(portIndex, DefaultImageUrl);
    }

    // Focus management
    onClick() {
        // TODO: Double click for edition?
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

    // Resizeable
    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    // Resizeable
    resize(dim: { width: number; height: number; }) {
        // Check that what the minimum available size is

        this.width = Math.max(this.minWidth, dim.width);
        this.height = Math.max(this.minHeight, dim.height);

        this.block.blockData.dimensions = { width: this.width, height: this.height };

        this._updateSize();
        this.handle.update();
    }

    // Handleable element
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


        if (settings.body && settings.body.image) {
            const imageUrl = this.block.workspace.getAssetUrlOnProgram(settings.body.image.id);
            this.imageBox.setAttributeNS(null, 'href', imageUrl);

            if (!settingsStorage.body) {
                settingsStorage.body = {};
            }
            settingsStorage.body.image = { id: settings.body.image.id };
        }

        this.block.blockData.settings = settingsStorage;
    }

    getCurrentConfiguration(): BlockConfigurationOptions {
        return Object.assign({}, this.block.blockData.settings || {});
    }

    getAllowedConfigurations(): BlockAllowedConfigurations {
        return {
            body: {
                image: true,
            },
        };
    }

    // Style management
    updateStyle() {
        const settings = this.block.blockData.settings;
        if (!settings) {
            return;
        }
    }

    // Aux
    _updateSize() {
        this.imageBox.setAttributeNS(null, 'width', this.width + "");
        this.imageBox.setAttributeNS(null, 'height', this.height + "");

        this.rect.setAttributeNS(null, 'height', this.height + "");
        this.rect.setAttributeNS(null, 'width', this.width + "");
    }
}
