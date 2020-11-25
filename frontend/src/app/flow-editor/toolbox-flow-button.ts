import { FlowActuator } from './flow_block';

export type ToolboxFlowButtonInitOps = {
    message: string,
    action: () => void,
};

export class ToolboxFlowButton implements FlowActuator {
    message: string;
    action: () => void;

    constructor(ops: ToolboxFlowButtonInitOps) {
        this.message = ops.message;
        this.action = ops.action;
    }

    render(div: HTMLDivElement): HTMLElement {
        const element = document.createElement('button');
        element.classList.add('toolbox-flow-button')
        element.innerText = this.message;

        div.appendChild(element);
        return element;
    }

    onclick(): void {
        return this.action();
    }

}
