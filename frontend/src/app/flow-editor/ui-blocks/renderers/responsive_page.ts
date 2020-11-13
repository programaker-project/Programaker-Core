import { Subscription } from "rxjs";
import { UiSignalService } from "../../../services/ui-signal.service";
import { FlowBlock, Resizeable, Area2D } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockHandler, UiFlowBlockBuilderInitOps } from "../ui_flow_block";
import { getRefBox } from "./utils";
import { ContainerElement, ContainerElementHandle } from "./container_element_handle";


const SvgNS = "http://www.w3.org/2000/svg";
const Title = "Responsive page";

export const ResponsivePageBuilder : UiFlowBlockBuilder = (canvas: SVGElement,
                                                           group: SVGElement,
                                                           block: UiFlowBlock,
                                                           service: UiSignalService,
                                                           initOps: UiFlowBlockBuilderInitOps,
                                                          ) => {
    const element = new ResponsivePage(canvas, group, block, service, initOps);
    element.init();
    return element;
}

class ResponsivePage implements UiFlowBlockHandler, ContainerElement, Resizeable {
    subscription: Subscription;
    text: SVGTextElement;
    handle: ContainerElementHandle | null = null;
    node: SVGAElement;
    textDim: { width: number; height: number; };
    rect: SVGRectElement;
    rectShadow: SVGRectElement;

    constructor(canvas: SVGElement, group: SVGElement,
                private block: UiFlowBlock,
                private service: UiSignalService,
                private initOps: UiFlowBlockBuilderInitOps) {

        const refBox = getRefBox(canvas);

        this.node = document.createElementNS(SvgNS, 'a');
        this.rect = document.createElementNS(SvgNS, 'rect');
        this.rectShadow = document.createElementNS(SvgNS, 'rect');

        group.setAttribute('class', 'flow_node ui_node container_node responsive_page');

        this.text = document.createElementNS(SvgNS, 'text');
        this.text.setAttribute('class', 'output_text');
        this.text.setAttributeNS(null,'textlength', '100%');

        this.text.textContent = Title;

        this.node.appendChild(this.rectShadow);
        this.node.appendChild(this.rect);
        this.node.appendChild(this.text);
        group.appendChild(this.node);

        const text_width = this.text.getClientRects()[0].width;
        const text_height = this.text.getClientRects()[0].height;
        this.textDim = { width: text_width, height: text_height };


        let box_height = refBox.height * 10;
        let box_width = text_width * 1.25;
        if (block.blockData.dimensions) {
            box_height = block.blockData.dimensions.height;
            box_width = block.blockData.dimensions.width;
        }

        this.text.setAttributeNS(null, 'y', box_height/2 - text_height / 2 + "");
        this.text.setAttributeNS(null, 'x', (box_width - text_width)/2 + "");

        this.rect.setAttributeNS(null, 'class', "node_body");
        this.rect.setAttributeNS(null, 'x', "0");
        this.rect.setAttributeNS(null, 'y', "0");
        this.rect.setAttributeNS(null, 'height', box_height + "");
        this.rect.setAttributeNS(null, 'width', box_width + "");

        this.rectShadow.setAttributeNS(null, 'class', "body_shadow");
        this.rectShadow.setAttributeNS(null, 'x', "0");
        this.rectShadow.setAttributeNS(null, 'y', "0");
        this.rectShadow.setAttributeNS(null, 'height', box_height + "");
        this.rectShadow.setAttributeNS(null, 'width', box_width + "");


        if (initOps.workspace) {
            this.handle = new ContainerElementHandle(this, initOps.workspace);
        }
    }

    init() {
        if (this.handle) {
            this.handle.init();
        }
    }

    // Resizeable
    resize(dim: { width: number; height: number; }) {
        const width = Math.max(this.textDim.width, dim.width);
        const height = Math.max(this.textDim.height, dim.height);

        this.rect.setAttributeNS(null, 'width', width + '');
        this.rect.setAttributeNS(null, 'height', height + '');

        this.rectShadow.setAttributeNS(null, 'width', width + '');
        this.rectShadow.setAttributeNS(null, 'height', height + '');

        this.text.setAttributeNS(null, 'x', (width - this.textDim.width)/2 + "");
        this.text.setAttributeNS(null, 'y', (height/2 + this.textDim.height/2) + "");

        this.block.blockData.dimensions = { width, height };

        this.handle.update();
    }

    getBodyArea(): Area2D {
        return this.block.getBodyArea();
    }

    // UiFlowBlock
    onClick() {
        if (!this.handle) {
            throw new Error("Cannot show manipulators as workspace has not been received.");
        }
        else {
            this.handle.show();
        }
    }

    dispose() {}

    onInputUpdated(connectedBlock: FlowBlock, inputIndex: number) {}

    onConnectionLost(portIndex: number) {}

    onConnectionValueUpdate(_inputIndex: number, value: string) {}

    // Container element
    getBodyElement(): SVGElement {
        return this.node;
    }

    getBlock(): FlowBlock {
        return this.block;
    }

}
