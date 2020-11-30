import { Position2D, FlowBlock, Resizeable } from "../../flow_block";
import { FlowWorkspace } from "../../flow_workspace";

const SvgNS = "http://www.w3.org/2000/svg";

const ManipulatorButtonSize = '200';
const HEIGHT_MANIPULATOR_VERTICAL_PADDING = 10;
const RESIZE_MANIPULATOR_SIZE = 25;
const MANIPULATOR_ICON_PADDING = 5;

export type HandleOption
    = 'resize_width_height'
    | 'resize_height'
;

export interface ContainerElement {
    getBodyElement: () => SVGElement;
    getBlock: () => FlowBlock;
}

function gen_width_height_resize_icon(size: number): SVGElement {
    const element = document.createElementNS(SvgNS, 'path');

    const far = size - MANIPULATOR_ICON_PADDING;
    const near = MANIPULATOR_ICON_PADDING;

    element.setAttributeNS(null, 'd', `M${far},${far} V${near} C ${far},${near} ${far * .75},${far * .75} ${near},${far} Z`);

    return element;
}

function gen_height_resize_icon(size: number): SVGElement {
    const element = document.createElementNS(SvgNS, 'path');

    const center = size/2;
    const far = size - MANIPULATOR_ICON_PADDING;
    const near = MANIPULATOR_ICON_PADDING;

    const cols = (size - MANIPULATOR_ICON_PADDING * 2) / 3;

    const col1 = MANIPULATOR_ICON_PADDING + cols;
    const col2 = col1 + cols;

    element.setAttributeNS(null, 'd', `M${col1},${near} V${center} H${near} L${center},${far} L${far},${center} H${col2} V${near} H${far} H${near} `);

    return element;
}

export class ContainerElementHandle {
    handleGroup: SVGGElement;
    widthHeightResizeManipulator: SVGElement;
    body: SVGElement;
    resizePrevPos: Position2D;
    heightResizeManipulator: SVGGElement;

    constructor(private element: ContainerElement,
                private workspace: FlowWorkspace,
                private handleOptions: HandleOption[]) {}

    init() {
        this.body = this.element.getBodyElement();
        this.handleGroup = document.createElementNS(SvgNS, 'g');

        this.handleGroup.setAttribute('class', 'manipulators hidden');

        if (this.handleOptions.indexOf('resize_width_height') >= 0) {
            this.widthHeightResizeManipulator = document.createElementNS(SvgNS, 'g');

            const iconBackground = document.createElementNS(SvgNS, 'rect');
            iconBackground.setAttribute('class', 'handle-icon-background');
            iconBackground.setAttributeNS(null, 'rx', '4');
            iconBackground.setAttributeNS(null, 'width', RESIZE_MANIPULATOR_SIZE + '');
            iconBackground.setAttributeNS(null, 'height', RESIZE_MANIPULATOR_SIZE + '');
            this.widthHeightResizeManipulator.appendChild(iconBackground);

            const resizeIcon = gen_width_height_resize_icon(RESIZE_MANIPULATOR_SIZE);
            this.widthHeightResizeManipulator.appendChild(resizeIcon);
            this.widthHeightResizeManipulator.onmousedown = this._startResizeWidthHeight.bind(this);

            this.widthHeightResizeManipulator.setAttribute('class', 'resize-manipulator width-height-resizer');
            this.handleGroup.appendChild(this.widthHeightResizeManipulator);
        }
        else if (this.handleOptions.indexOf('resize_height') >= 0) {
            this.heightResizeManipulator = document.createElementNS(SvgNS, 'g');

            const iconBackground = document.createElementNS(SvgNS, 'rect');
            iconBackground.setAttribute('class', 'handle-icon-background');
            iconBackground.setAttributeNS(null, 'rx', '4');
            iconBackground.setAttributeNS(null, 'width', RESIZE_MANIPULATOR_SIZE + '');
            iconBackground.setAttributeNS(null, 'height', RESIZE_MANIPULATOR_SIZE + '');
            this.heightResizeManipulator.appendChild(iconBackground);

            const resizeIcon = gen_height_resize_icon(RESIZE_MANIPULATOR_SIZE);
            this.heightResizeManipulator.appendChild(resizeIcon);
            this.heightResizeManipulator.onmousedown = this._startResizeHeight.bind(this);

            this.heightResizeManipulator.setAttribute('class', 'resize-manipulator height-resizer');
            this.handleGroup.appendChild(this.heightResizeManipulator);
        }

        this.body.parentNode.appendChild(this.handleGroup); // Avoid the manipulators affecting the element
        this._reposition();
    }

    show() {
        this.handleGroup.classList.remove('hidden');
    }

    hide() {
        this.handleGroup.classList.add('hidden');
    }

    update() {
        this._reposition();
    }

    private _startResizeWidthHeight(ev: MouseEvent) {
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        this.workspace.startResizing(this.element as any as Resizeable, ev);

        return true;
    }

    private _startResizeHeight(ev: MouseEvent) {
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        // TODO: Add resize type
        this.workspace.startResizing(this.element as any as Resizeable, ev);

        return true;
    }

    private _reposition() {
        const box = this.body.getClientRects()[0];
        if (this.widthHeightResizeManipulator) {
            this.widthHeightResizeManipulator.setAttributeNS(null, 'transform', `translate(${box.width}, ${box.height})`);
        }

        if (this.heightResizeManipulator) {
            this.heightResizeManipulator.setAttributeNS(null, 'transform', `translate(${box.width / 2}, ${box.height + HEIGHT_MANIPULATOR_VERTICAL_PADDING})`);
        }
    }
}
