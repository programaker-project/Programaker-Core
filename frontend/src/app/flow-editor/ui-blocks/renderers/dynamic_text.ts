import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { DirectValue } from "../../direct_value";
import { FlowBlock } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockHandler, TextEditable, TextReadable, UiFlowBlockBuilderInitOps } from "../ui_flow_block";
import { UiElementHandle, HandleableElement, ConfigurableSettingsElement } from "./ui_element_handle";
import { BlockAllowedConfigurations, BlockConfigurationOptions } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";


const SvgNS = "http://www.w3.org/2000/svg";
const DefaultContent = "- Dynamic text -";
const DEFAULT_BACKGROUND_COLOR = '#222';
const DEFAULT_TEXT_COLOR = '#fc4';

export const DynamicTextBuilder : UiFlowBlockBuilder = (canvas: SVGElement,
                                                        group: SVGElement,
                                                        block: UiFlowBlock,
                                                        service: UiSignalService,
                                                        initOps: UiFlowBlockBuilderInitOps,
                                                       ) => {
    const output = new DynamicText(canvas, group, block, service, initOps);
    output.init();
    return output;
}

class DynamicText implements UiFlowBlockHandler, ConfigurableSettingsElement, HandleableElement {
    private subscription: Subscription;
    private textBox: SVGTextElement;
    private rect: SVGRectElement;
    private rectShadow: SVGRectElement;
    private handle: UiElementHandle;
    readonly MinWidth = 120;
    isStaticText: boolean;
    private value: string;

    constructor(canvas: SVGElement, group: SVGElement,
                private block: UiFlowBlock,
                private service: UiSignalService,
                private initOps: UiFlowBlockBuilderInitOps) {

        const node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node output_node');

        this.textBox = document.createElementNS(SvgNS, 'text');
        this.textBox.setAttribute('class', 'output_text');
        this.textBox.setAttributeNS(null,'textlength', '100%');

        this.value = DefaultContent;

        node.appendChild(this.rectShadow);
        node.appendChild(this.rect);
        node.appendChild(this.textBox);
        group.appendChild(node);

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");

        this.updateStyle();
        this._updateText();
        this._updateSize();

        if (initOps.workspace) {
            this.handle = new UiElementHandle(this, initOps.workspace, ['adjust_settings']);
        }
    }

    isTextEditable(): this is TextEditable {
        return false;
    }

    isTextReadable(): this is TextReadable {
        return true;
    }

    get text(): string {
        return this.value;
    }

    dispose() {
        return () => this.subscription.unsubscribe();
    }

    onInputUpdated(connectedBlock: FlowBlock, inputIndex: number) {
        this.isStaticText = connectedBlock instanceof DirectValue;
        if (connectedBlock instanceof DirectValue) {
            this.onConnectionValueUpdate(inputIndex, connectedBlock.value);
        }
    }

    onConnectionLost(portIndex: number) {
        this.onConnectionValueUpdate(portIndex, DefaultContent);
    }

    onConnectionValueUpdate(_inputIndex: number, value: string) {
        this.value = value;
        this._updateText();
        this._updateSize();
    }

    init() {
        const observer = this.service.onElementUpdate(this.block.options.id, this.block.id);

        this.subscription = observer.subscribe({
            next: (value: any) => {
                this.onConnectionValueUpdate(0, JSON.stringify(value.values[0]));
            }
        });

        if (this.handle) {
            this.handle.init();
        }
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

        settingsStorage.bg = settings.bg;

        this.block.blockData.settings = settingsStorage;
        this.updateStyle();
        this._updateSize({ anchor: 'bottom-center' }); // Style changes might change the block's size

        if (this.handle) {
            this.handle.update();
        }
    }

    getCurrentConfiguration(): BlockConfigurationOptions {
        const config = Object.assign({}, this.block.blockData.settings || {});

        // Seed default configuration if not already there
        if (!config.bg) {
            config.bg = { type: 'color', value: DEFAULT_BACKGROUND_COLOR };
        }
        if (!config.text) {
            config.text = {};
        }
        if (!config.text.color) {
            config.text.color = {value: DEFAULT_TEXT_COLOR};
        }

        return config;
    }

    getAllowedConfigurations(): BlockAllowedConfigurations {
        return {
            text: {
                color: true,
                fontSize: true,
            },
            background: {
                color: true,
                image: false,
            }
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

        if (settings.bg) {
            // Get color to apply
            let color = DEFAULT_BACKGROUND_COLOR;
            if (settings.bg.type === 'color') {
                color = settings.bg.value;
            }
            else if (settings.bg.type === 'transparent') {
                color = 'transparent';
            }

            // The shadow creates unexpected effects with transparent
            // backgrounds, better to just remove it
            if (color === 'transparent') {
                this.rectShadow.classList.add('hidden');
            }
            else {
                this.rectShadow.classList.remove('hidden');
            }

            // Apply it to the element's background
            this.rect.style.fill = color;
        }
    }

    // Aux
    _updateText() {
        this.textBox.innerHTML = '';

        const lines = this.value.split('\n')
        for (let line of lines) {
            if (line.length === 0) {
                line = ' '
            }
            const span = document.createElementNS(SvgNS, 'tspan');
            span.setAttributeNS(null, 'x', '0');
            span.setAttributeNS(null, 'dy', '1.2em');
            span.textContent = line;

            this.textBox.appendChild(span);
        }
    }

    _updateSize(opts?: { anchor?: 'bottom-center' | 'top-left' }) {
        const textArea = this.textBox.getClientRects()[0];

        const box_height = textArea.height * 1.5;
        const box_width = Math.max(textArea.width + 50, this.MinWidth);

        this.textBox.setAttributeNS(null, 'y', (box_height - textArea.height)/2 + "");
        for (const line of Array.from(this.textBox.childNodes)) {
            if (line instanceof SVGTSpanElement) {
                line.setAttributeNS(null, 'x', (box_width - textArea.width)/2 + "");
            }
        }

        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");
    }
}
