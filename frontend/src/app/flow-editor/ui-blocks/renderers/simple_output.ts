import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { DirectValue } from "../../direct_value";
import { FlowBlock } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockHandler } from "../ui_flow_block";
import { getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const DefaultContent = "- No content -";

export const SimpleOutputBuilder : UiFlowBlockBuilder = (canvas: SVGElement, group: SVGElement, block: UiFlowBlock, service: UiSignalService) => {
    const output = new SimpleOutput(canvas, group, block, service);
    output.init();
    return output;
}

class SimpleOutput implements UiFlowBlockHandler {
    subscription: Subscription;
    text: SVGTextElement;

    constructor(canvas: SVGElement, group: SVGElement,
                private block: UiFlowBlock,
                private service: UiSignalService) {

        const refBox = getRefBox(canvas);

        const node = document.createElementNS(SvgNS, 'a');
        const rect = document.createElementNS(SvgNS, 'rect');
        const rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node output_node');

        this.text = document.createElementNS(SvgNS, 'text');
        this.text.setAttribute('class', 'output_text');
        this.text.setAttributeNS(null,'textlength', '100%');

        this.text.textContent = DefaultContent;

        node.appendChild(rectShadow);
        node.appendChild(rect);
        node.appendChild(this.text);
        group.appendChild(node);

        const text_width = this.text.getClientRects()[0].width;

        const box_height = refBox.height * 2;
        const box_width = text_width * 1.5;

        this.text.setAttributeNS(null, 'y', box_height/1.5 + "");
        this.text.setAttributeNS(null, 'x', (box_width - text_width)/2 + "");

        rect.setAttributeNS(null, 'class', "node_body");
        rect.setAttributeNS(null, 'x', "0");
        rect.setAttributeNS(null, 'y', "0");
        rect.setAttributeNS(null, 'height', box_height + "");
        rect.setAttributeNS(null, 'width', box_width + "");

        rectShadow.setAttributeNS(null, 'class', "body_shadow");
        rectShadow.setAttributeNS(null, 'x', "0");
        rectShadow.setAttributeNS(null, 'y', "0");
        rectShadow.setAttributeNS(null, 'height', box_height + "");
        rectShadow.setAttributeNS(null, 'width', box_width + "");
    }

    onClick() {
        console.log("Unhandled click on output screen:", this.block);
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
        this.text.textContent = value;
    }

    init() {
        // Nothing to do here... yet
        const observer = this.service.onElementUpdate(this.block.options.id, this.block.id);

        this.subscription = observer.subscribe({
            next: (value: any) => {
                this.text.textContent = JSON.stringify(value.values[0]);
            }
        })
    }
}
