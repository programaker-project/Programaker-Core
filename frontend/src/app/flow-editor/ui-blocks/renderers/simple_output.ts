import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { DirectValue } from "../../direct_value";
import { FlowBlock } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockHandler, TextEditable, TextReadable } from "../ui_flow_block";
import { getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const DefaultContent = "- No content -";

export const SimpleOutputBuilder : UiFlowBlockBuilder = (canvas: SVGElement, group: SVGElement, block: UiFlowBlock, service: UiSignalService) => {
    const output = new SimpleOutput(canvas, group, block, service);
    output.init();
    return output;
}

class SimpleOutput implements UiFlowBlockHandler {
    private subscription: Subscription;
    private textBox: SVGTextElement;
    private rect: SVGRectElement;
    private rectShadow: SVGRectElement;

    constructor(canvas: SVGElement, group: SVGElement,
                private block: UiFlowBlock,
                private service: UiSignalService) {

        const refBox = getRefBox(canvas);

        const node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node output_node');

        this.textBox = document.createElementNS(SvgNS, 'text');
        this.textBox.setAttribute('class', 'output_text');
        this.textBox.setAttributeNS(null,'textlength', '100%');

        this.textBox.textContent = DefaultContent;

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

        this._updateSize();
    }

    onClick() {
        console.log("Unhandled click on output screen:", this.block);
    }

    isTextEditable(): this is TextEditable {
        return false;
    }

    isTextReadable(): this is TextReadable {
        return true;
    }

    get text(): string {
        return this.textBox.textContent;
    }

    dispose() {
        return () => this.subscription.unsubscribe();
    }

    onInputUpdated(connectedBlock: FlowBlock, inputIndex: number) {
        if (connectedBlock instanceof DirectValue) {
            this.onConnectionValueUpdate(inputIndex, connectedBlock.value);
        }
    }

    onConnectionLost(portIndex: number) {
        this.onConnectionValueUpdate(portIndex, DefaultContent);
    }

    onConnectionValueUpdate(_inputIndex: number, value: string) {
        this.textBox.textContent = value;
        this._updateSize();
    }

    init() {
        // Nothing to do here... yet
        const observer = this.service.onElementUpdate(this.block.options.id, this.block.id);

        this.subscription = observer.subscribe({
            next: (value: any) => {
                this.textBox.textContent = JSON.stringify(value.values[0]);
            }
        })
    }


    // Aux
    _updateSize() {
        const textArea = this.textBox.getClientRects()[0];

        const box_height = textArea.height * 2;
        const box_width = textArea.width * 1.5;

        this.textBox.setAttributeNS(null, 'y', box_height/1.5 + "");
        this.textBox.setAttributeNS(null, 'x', (box_width - textArea.width)/2 + "");

        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");
    }
}
