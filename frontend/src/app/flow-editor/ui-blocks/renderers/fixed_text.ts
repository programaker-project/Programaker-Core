import { UiSignalService } from "../../../services/ui-signal.service";
import { Area2D, FlowBlock } from "../../flow_block";
import { TextEditable, TextReadable, UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockBuilderInitOps, UiFlowBlockHandler, Autoresizable } from "../ui_flow_block";
import { ConfigurableSettingsElement, HandleableElement, UiElementHandle } from "./ui_element_handle";
import { BlockConfigurationOptions, BlockAllowedConfigurations, fontWeightToCss } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";
import { ContainerFlowBlock } from "../container_flow_block";
import { startOnElementEditor, FormattedTextTree, formattedTextTreeToDom } from "./utils";
import { FlowWorkspace } from "../../flow_workspace";



const SvgNS = "http://www.w3.org/2000/svg";
const MAX_WIDTH = 1024;

const DefaultContent =  { type: 'text', value: "- Static (editable) text -"};

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
    private textBox: SVGForeignObjectElement;
    private textValue: FormattedTextTree;
    private rect: SVGRectElement;
    readonly MinWidth = 140;
    readonly MinHeight = 140;
    private handle: UiElementHandle;
    private _container: ContainerFlowBlock;
    private textArea: Area2D;
    private contentBox: HTMLDivElement;
    private editing = false;

    private readonly workspace: FlowWorkspace;

    constructor(canvas: SVGElement, group: SVGElement,
        private block: UiFlowBlock,
        private service: UiSignalService,
        private initOps: UiFlowBlockBuilderInitOps) {

        const node = document.createElementNS(SvgNS, 'g');
        this.rect = document.createElementNS(SvgNS, 'rect');
        const contentsGroup = document.createElementNS(SvgNS, 'g');

        group.setAttribute('class', 'flow_node ui_node text_node');

        this.textBox = document.createElementNS(SvgNS, 'foreignObject');
        this.textBox.setAttribute('class', 'text');

        if ((block.blockData.textContent) && !(block.blockData.content)) {
            block.blockData.content = [{ type: 'text', value: block.blockData.textContent }];
        }
        this.textValue = block.blockData.content || [DefaultContent];

        contentsGroup.appendChild(this.textBox);
        node.appendChild(this.rect);
        node.appendChild(this.textBox);
        group.appendChild(node);


        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");

        this.updateStyle();
        this._updateTextBox();
        this._updateSize();

        if (initOps.workspace) {
            this.workspace = initOps.workspace;
            this.handle = new UiElementHandle(this, node, initOps.workspace, ['adjust_settings']);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }


    getArea(): Area2D {
        return this.getBodyElement().getBBox();
    }

    isTextEditable(): this is TextEditable {
        return false;
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
        return this.contentBox.innerText;
    }

    dispose() {}

    onInputUpdated(block: FlowBlock, inputIndex: number) {}

    onConnectionValueUpdate(inputIndex: number, value: string) {}

    onConnectionLost(portIndex: number) { }

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
        if (this.contentBox) {
            this.contentBox.blur();
        }
        if (!this.handle) {
            throw new Error("Cannot show manipulators as workspace has not been received.");
        }
        else {
            this.handle.hide();
        }
    }

    // Handleable element
    doesTakeAllHorizontal() {
        return false;
    }

    isAutoresizable(): this is Autoresizable {
        return false;
    }

    getMinSize() {
        return { width: this.MinWidth, height: this.MinHeight };
    }

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
            if (settings.text.fontWeight) {
                settingsStorage.text.fontWeight = {value: settings.text.fontWeight.value};
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
                fontWeight: true,
            },
        };
    }

    // When inside a container, avoid overflowing it
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
        if (!this.editing) {
            this._updateSize();
        }
        return { x: 0, y: 0 };
    }

    // Style management
    updateStyle() {
        const settings = this.block.blockData.settings;
        if (!settings) {
            return;
        }

        if (settings.text) {
            if (settings.text.color) {
                this.textBox.style.color = settings.text.color.value;
            }
            if (settings.text.fontSize) {
                this.textBox.style.fontSize = settings.text.fontSize.value + 'px';
            }
            if (settings.text.fontWeight) {
                this.textBox.style.fontWeight = fontWeightToCss(settings.text.fontWeight.value);
            }
        }
    }

    // Aux
    onContentEditStart() {
        this.textBox.setAttributeNS(null, 'y', "");
        this.textBox.setAttributeNS(null, 'x', "");

        const width = this.rect.getAttributeNS(null, 'width');
        const height = this.rect.getAttributeNS(null, 'height');
        this.textBox.setAttributeNS(null, 'width', width);
        this.textBox.setAttributeNS(null, 'height', height);

        this.contentBox.style.height = height + 'px';
        this.contentBox.style.maxWidth = '';
        this.contentBox.style.width = '100%';
        this.contentBox.style.height = '100%';
        this.contentBox.style.overflow = 'auto';
        this.contentBox.classList.add('editing');

        this.editing = true;

        startOnElementEditor(this.contentBox, this.textBox, this.block.workspace.getDialog(),
                             (tt: FormattedTextTree) => {
                                 this.block.blockData.content = this.textValue = tt;

                                 this.editing = false;
                                 this._updateTextBox();
                                 this._updateSize();
                                 if (this.workspace) {
                                     this.workspace.invalidateBlock(this.block.id);
                                 }
                             },
                             (width: number, height: number) => {
                                 width = Math.max(width, this.MinWidth);
                                 height = Math.max(height, this.MinHeight);

                                 const zoom = this.workspace ? this.workspace.getInvZoomLevel() : 1;

                                 this.rect.setAttributeNS(null, 'width', width * zoom + '');
                                 this.rect.setAttributeNS(null, 'height', height * zoom + '');
                                 this.textBox.setAttributeNS(null, 'width', width * zoom + '');
                                 this.textBox.setAttributeNS(null, 'height', height * zoom + '');
                             });
    }

    _updateTextBox() {
        this.textBox.innerHTML = '';

        this.contentBox = document.createElement('div');
        this.contentBox.style.width = 'max-content';

        if (this.initOps.workspace) {
            // Don't make editable on exhibitor
            this.contentBox.contentEditable = 'true';
        }
        this.contentBox.onfocus = this.onContentEditStart.bind(this);
        this.contentBox.onmousedown = (ev: MouseEvent) => {
            ev.stopImmediatePropagation();
        }

        const container = document.createElement('div');
        const content = formattedTextTreeToDom(this.textValue);

        container.appendChild(content);
        this.contentBox.appendChild(container);
        this.textBox.appendChild(this.contentBox);

        // Then mark that as max-width
        this.contentBox.style.maxWidth = 'max-content';
        this.contentBox.style.width = '';

        this._updateTextArea();
    }

    _updateTextArea() {
        const textArea = this.contentBox.getClientRects()[0];
        const zoom = this.workspace ? this.workspace.getInvZoomLevel() : 1;

        this.textArea = {
            x: textArea.x,
            y: textArea.y,
            width: textArea.width * zoom,
            height: textArea.height * zoom,
        };
    }

    _updateSize(opts?: { anchor?: 'bottom-center' | 'top-left' }) {
        // Obtain size taken by all the text
        const zoom = this.workspace ? this.workspace.getInvZoomLevel() : 1;
        let maxWidth = MAX_WIDTH;

        const anchor = opts && opts.anchor ? opts.anchor : 'top-left';

        const oldHeight = this.rect.height.baseVal.value;
        const oldWidth = this.rect.width.baseVal.value;
        const box_height = Math.max(this.textArea.height + 50 * zoom, this.MinHeight);
        const box_width = Math.min(maxWidth, Math.max(this.textArea.width + 50 * zoom, this.MinWidth));

        if (anchor === 'bottom-center') {
            // Move the box around to respect the anchor point
            this.block.moveBy({
                x: -((box_width - oldWidth) / 2),
                y: -(box_height - oldHeight),
            })
        }

        this.textBox.setAttributeNS(null, 'width', box_width + "");
        this.textBox.setAttributeNS(null, 'height', this.textArea.height + "");

        this._updateTextArea();

        this.textBox.setAttributeNS(null, 'x', (box_width - this.textArea.width)/2 + "");
        this.textBox.setAttributeNS(null, 'y', (box_height - this.textArea.height)/2 + "");
        this.textBox.setAttributeNS(null, 'height', this.textArea.height + "");

        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");

        if (this.handle) {
            this.handle.update();
        }
    }
}
