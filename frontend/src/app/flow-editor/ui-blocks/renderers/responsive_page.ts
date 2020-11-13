import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { FlowBlock } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockHandler } from "../ui_flow_block";
import { getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const Title = "Responsive page";

export const ResponsivePageBuilder : UiFlowBlockBuilder = (canvas: SVGElement, group: SVGElement, block: UiFlowBlock, service: UiSignalService) => {
    return new ResponsivePage(canvas, group, block, service);
}

class ResponsivePage implements UiFlowBlockHandler {
    subscription: Subscription;
    text: SVGTextElement;

    constructor(canvas: SVGElement, group: SVGElement,
                private block: UiFlowBlock,
                private service: UiSignalService) {

        const refBox = getRefBox(canvas);

        const node = document.createElementNS(SvgNS, 'a');
        const rect = document.createElementNS(SvgNS, 'rect');
        const rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node container_node responsive_page');

        this.text = document.createElementNS(SvgNS, 'text');
        this.text.setAttribute('class', 'output_text');
        this.text.setAttributeNS(null,'textlength', '100%');

        this.text.textContent = Title;

        node.appendChild(rectShadow);
        node.appendChild(rect);
        node.appendChild(this.text);
        group.appendChild(node);

        const text_width = this.text.getClientRects()[0].width;
        const text_height = this.text.getClientRects()[0].height;

        const box_height = refBox.height * 10;
        const box_width = text_width * 1.25;

        this.text.setAttributeNS(null, 'y', box_height/2 - text_height / 2 + "");
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
        console.log("Unhandled click on", this);
    }

    dispose() {}

    onInputUpdated(connectedBlock: FlowBlock, inputIndex: number) {}

    onConnectionLost(portIndex: number) {}

    onConnectionValueUpdate(_inputIndex: number, value: string) {}
}
