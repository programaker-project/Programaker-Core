import { Position2D, FlowBlock, Resizeable } from "../../flow_block";
import { FlowWorkspace } from "../../flow_workspace";

const SvgNS = "http://www.w3.org/2000/svg";

const ManipulatorButtonSize = '200';

export interface ContainerElement {
    getBodyElement: () => SVGElement;
    getBlock: () => FlowBlock;
}

function gen_resize_icon(size: number): SVGElement {
    const element = document.createElementNS(SvgNS, 'path');

    element.setAttributeNS(null, 'd', `M${size},${size} V0 C ${size},0 ${size * .75},${size * .75} 0,${size} Z`);

    return element;
}

export class ContainerElementHandle {
    handleGroup: SVGGElement;
    resizeManipulator: SVGElement;
    body: SVGElement;
    resizePrevPos: Position2D;

    constructor(private element: ContainerElement,
                private workspace: FlowWorkspace) {}

    init() {
        this.body = this.element.getBodyElement();
        this.handleGroup = document.createElementNS(SvgNS, 'g');

        this.handleGroup.setAttribute('class', 'manipulators hidden');

        this.resizeManipulator = document.createElementNS(SvgNS, 'g');
        const resizeIcon = gen_resize_icon(25);
        this.resizeManipulator.appendChild(resizeIcon);
        this.resizeManipulator.onmousedown = this._startResize.bind(this);

        this.resizeManipulator.setAttribute('class', 'resize-manipulator');

        this.handleGroup.appendChild(this.resizeManipulator);

        this.body.parentNode.appendChild(this.handleGroup); // Avoid the manipulators affecting the element
        this._reposition();
    }

    show() {
        this.handleGroup.classList.remove('hidden');
    }

    update() {
        this._reposition();
    }

    private _startResize(ev: MouseEvent) {
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        this.workspace.startResizing(this.element as any as Resizeable, ev);

        return true;
    }

    private _reposition() {
        const box = this.body.getClientRects()[0];
        this.resizeManipulator.setAttributeNS(null, 'transform', `translate(${box.width}, ${box.height})`)
    }
}
