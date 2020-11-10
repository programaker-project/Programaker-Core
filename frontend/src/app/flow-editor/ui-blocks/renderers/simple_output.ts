import { UiSignalService } from "../../../services/ui-signal.service";
import { OnUiFlowBlockClick, UiFlowBlock, UiFlowBlockRenderer } from "../ui_flow_block";
import { getRefBox } from "./utils";


const SvgNS = "http://www.w3.org/2000/svg";
const DefaultContent = "- No content yet -";

type Handler = SVGTextElement;

export const SimpleOutputRenderer: UiFlowBlockRenderer = (canvas: SVGElement, group: SVGElement, block: UiFlowBlock, service: UiSignalService) => {
    const refBox = getRefBox(canvas);

    const node = document.createElementNS(SvgNS, 'a');
    const rect = document.createElementNS(SvgNS, 'rect');
    const rectShadow = document.createElementNS(SvgNS, 'rect');
    const contentsGroup = document.createElementNS(SvgNS, 'g');


    group.setAttribute('class', 'flow_node ui_node output_node');

    const text = document.createElementNS(SvgNS, 'text');
    text.setAttribute('class', 'output_text');
    text.setAttributeNS(null,'textlength', '100%');

    text.textContent = DefaultContent;

    contentsGroup.appendChild(text);
    node.appendChild(rectShadow);
    node.appendChild(rect);
    node.appendChild(text);
    group.appendChild(node);

    const text_width = text.getClientRects()[0].width;

    const box_height = refBox.height * 2;
    const box_width = text_width * 1.5;

    text.setAttributeNS(null, 'y', box_height/1.5 + "");
    text.setAttributeNS(null, 'x', (box_width - text_width)/2 + "");

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

    return setup(block, service, text);
}

export const SimpleOutputOnClick : OnUiFlowBlockClick = (block: UiFlowBlock, service: UiSignalService) => {
    console.log("Unhandled click on output screen:", block);
}

export const setup = (block: UiFlowBlock, service: UiSignalService, handler: Handler) => {
    // Nothing to do here... yet
    const observer = service.onElementUpdate(block.options.id, block.id);

    const subscription = observer.subscribe({
        next: (value: any) => {
            handler.textContent = JSON.stringify(value.values[0]);
        }
    })

    return () => subscription.unsubscribe();
}
