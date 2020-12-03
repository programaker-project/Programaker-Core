import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock } from "../../flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockBuilderInitOps, UiFlowBlockHandler } from "../ui_flow_block";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { BlockConfigurationOptions, BlockAllowedConfigurations } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";


const SvgNS = "http://www.w3.org/2000/svg";

const DefaultContent = "Right click me to edit this text!";

export const FixedTextBuilder: UiFlowBlockBuilder = (canvas: SVGElement,
    group: SVGElement,
    block: UiFlowBlock,
    service: UiSignalService,
    initOps: UiFlowBlockBuilderInitOps,
) => {
    const element = new FixedText(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class FixedText implements UiFlowBlockHandler, TextEditable, ConfigurableSettingsElement, HandleableElement {
    private textBox: SVGTextElement;
    private textValue: string;
    private rect: SVGRectElement;
    readonly MinWidth = 120;
    private handle: UiElementHandle;

    constructor(canvas: SVGElement, group: SVGElement,
        private block: UiFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        const node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        const contentsGroup = document.createElementNS(SvgNS, 'g');

        group.setAttribute('class', 'flow_node ui_node text_node');

        this.textBox = document.createElementNS(SvgNS, 'text');
        this.textBox.setAttribute('class', 'text');
        this.textBox.setAttributeNS(null, 'textlength', '100%');

        this.textValue = this.textBox.textContent = block.blockData.textContent || DefaultContent;
        this.block.blockData.textContent = this.textValue;

        contentsGroup.appendChild(this.textBox);
        node.appendChild(this.rect);
        node.appendChild(this.textBox);
        group.appendChild(node);


        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.updateStyle();
        this._updateSize();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, initOps.workspace, ['adjust_settings']);
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
        return true;
    }

    isTextReadable(): this is TextReadable {
        return true;
    }

    get isStaticText(): boolean {
        return true;
    }

    get editableTextName(): string {
        return 'contents';
    }

    public get text(): string {
        return this.textValue;
    }

    public set text(val: string) {
        this.textBox.textContent = this.block.blockData.textContent = this.textValue = val;
        this._updateSize();
    }

    dispose() {}

    onInputUpdated(block: FlowBlock, inputIndex: number) {}

    onConnectionValueUpdate(inputIndex: number, value: string) {}

    onConnectionLost(portIndex: number) {
        this.onConnectionValueUpdate(portIndex, DefaultContent);
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

    // Handleable element
    getBodyElement(): SVGElement {
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

        if (settings.text) {

            if (!settingsStorage.text) {
                settingsStorage.text = {};
            }

            if (settings.text.color) {
                settingsStorage.text.color = {value: settings.text.color.value};
            }
            if (settings.text.fontSize) {
                settingsStorage.text.fontSize = {value: settings.text.fontSize.value};
            }
        }

        this.block.blockData.settings = settingsStorage;
        this.updateStyle();
        this._updateSize({ anchor: 'bottom-center' }); // Style changes might change the block's size

        if (this.handle) {
            this.handle.update();
        }
    }

    getCurrentConfiguration(): BlockConfigurationOptions {
        return Object.assign({}, this.block.blockData.settings || {});
    }

    getAllowedConfigurations(): BlockAllowedConfigurations {
        return {
            text: {
                color: true,
                fontSize: true,
            },
        };
    }

    // Style management
    updateStyle() {
        const settings = this.block.blockData.settings;
        if (!settings) {
            return;
        }

        if (settings.text) {
            if (settings.text.color) {
                this.textBox.style.fill = settings.text.color.value;
            }
            if (settings.text.fontSize) {
                this.textBox.style.fontSize = settings.text.fontSize.value + 'px';
            }
        }
    }

    // Aux
    _updateSize(opts?: { anchor?: 'bottom-center' | 'top-left' }) {
        const textArea = this.textBox.getClientRects()[0];

        const anchor = opts && opts.anchor ? opts.anchor : 'top-left';

        const oldHeight = this.rect.height.baseVal.value;
        const oldWidth = this.rect.width.baseVal.value;
        const box_height = textArea.height * 3;
        const box_width = Math.max(textArea.width + 50, this.MinWidth);

        if (anchor === 'bottom-center') {
            // Move the box around to respect the anchor point
            this.block.moveBy({
                x: -((box_width - oldWidth) / 2),
                y: -(box_height - oldHeight),
            })
        }

        this.textBox.setAttributeNS(null, 'y', box_height/1.5 + "");
        this.textBox.setAttributeNS(null, 'x', (box_width - textArea.width)/2 + "");

        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");
    }
}
