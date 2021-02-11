import { UiSignalService } from "../../../services/ui-signal.service";
import { UiFlowBlock, UiFlowBlockHandler, UiFlowBlockBuilder, TextEditable, TextReadable } from "../ui_flow_block";
import { FlowBlock, Area2D } from "../../flow_block";


const SvgNS = "http://www.w3.org/2000/svg";

const DefaultContent = "Button";

export const SimpleButtonBuilder: UiFlowBlockBuilder = (canvas: SVGElement, group: SVGElement, block: UiFlowBlock, service: UiSignalService) => {
    return new SimpleButton(canvas, group, block, service);
}

class SimpleButton implements UiFlowBlockHandler, TextEditable {
    private textBox: SVGTextElement;
    private textValue: string;
    private rect: SVGRectElement;
    private rectShadow: SVGRectElement;
    readonly MinWidth = 120;

    constructor(canvas: SVGElement, group: SVGElement,
        private block: UiFlowBlock,
        private service: UiSignalService) {
        const node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');
        const contentsGroup = document.createElementNS(SvgNS, 'g');


        group.setAttribute('class', 'flow_node ui_node button_node');

        this.textBox = document.createElementNS(SvgNS, 'text');
        this.textBox.setAttribute('class', 'button_text');
        this.textBox.setAttributeNS(null, 'textlength', '100%');

        this.textValue = this.textBox.textContent = block.blockData.textContent || DefaultContent;
        this.block.blockData.textContent = this.textValue;

        contentsGroup.appendChild(this.textBox);
        node.appendChild(this.rectShadow);
        node.appendChild(this.rect);
        node.appendChild(this.textBox);
        group.appendChild(node);


        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'rx', "5px"); // Like border-radius, in px

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'rx', "5px"); // Like border-radius, in px

        this._updateSize();
    }

    getArea(): Area2D {
        return this.rect.getBBox();
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    getBodyElement(): SVGRectElement {
        return this.rect;
    }


    onClick() {
        // return this.service.sendBlockSignal(this.block.options.id, this.block.id);
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
        return 'label';
    }

    public get text(): string {
        return this.textValue;
    }

    public set text(val: string) {
        this.textBox.textContent = this.block.blockData.textContent = this.textValue = val;
        this._updateSize();
    }

    updateOptions() {
        this.textValue = this.textBox.textContent = this.block.blockData.textContent || DefaultContent;
        this.block.blockData.textContent = this.textValue;
        this._updateSize();
    }

    dispose() {}

    onInputUpdated(block: FlowBlock, inputIndex: number) {}

    onConnectionValueUpdate(inputIndex: number, value: string) {}

    onConnectionLost(portIndex: number) {
        this.onConnectionValueUpdate(portIndex, DefaultContent);
    }

    // Aux
    _updateSize() {
        const textArea = this.textBox.getClientRects()[0];

        const box_height = textArea.height * 3;
        const box_width = Math.max(textArea.width + 50, this.MinWidth);

        this.textBox.setAttributeNS(null, 'y', box_height/1.5 + "");
        this.textBox.setAttributeNS(null, 'x', (box_width - textArea.width)/2 + "");

        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");
    }
}
