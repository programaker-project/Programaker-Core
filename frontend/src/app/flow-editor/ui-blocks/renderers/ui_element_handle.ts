import { Position2D, FlowBlock, Resizeable } from "../../flow_block";
import { FlowWorkspace } from "../../flow_workspace";
import { ConfigurableBlock } from "../../dialogs/configure-block-dialog/configure-block-dialog.component";

const SvgNS = "http://www.w3.org/2000/svg";

const ManipulatorButtonSize = '200';
const HEIGHT_MANIPULATOR_VERTICAL_PADDING = 10;
const WIDTH_MANIPULATOR_HORIZONTAL_PADDING = 10;
const RESIZE_MANIPULATOR_SIZE = 35;
const SETTINGS_MANIPULATOR_SIZE = 35;
const MANIPULATOR_ICON_PADDING = 5;

export type HandleOption
    = 'resize_width_height'
    | 'resize_height'
    | 'resize_width'
    | 'adjust_settings'
;

export interface HandleableElement {
    getBodyElement: () => SVGElement;
    getBlock: () => FlowBlock;
}

export interface ConfigurableSettingsElement extends ConfigurableBlock {
    startAdjustingSettings(): void;
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

function gen_width_resize_icon(size: number): SVGElement {
    const element = document.createElementNS(SvgNS, 'path');

    const center = size/2;
    const far = size - MANIPULATOR_ICON_PADDING;
    const near = MANIPULATOR_ICON_PADDING;

    const cols = (size - MANIPULATOR_ICON_PADDING * 2) / 3;

    const col1 = MANIPULATOR_ICON_PADDING + cols;
    const col2 = col1 + cols;

    element.setAttributeNS(null, 'd', `M${near},${col1} H${center} V${near} L${far},${center} L${center},${far} V${col2} H${near} V${far} V${near} `);

    return element;
}

function gen_settings_manipulator_icon(size: number): SVGElement {
    const element = document.createElementNS(SvgNS, 'image');

    element.setAttributeNS(null, 'href', '/assets/icons/settings.svg');
    element.setAttributeNS(null, 'width', size + '');
    element.setAttributeNS(null, 'height', size + '');

    return element;
}

export class UiElementHandle {
    handleGroup: SVGGElement;
    resizePrevPos: Position2D;
    settingsManipulator: SVGGElement;

    widthHeightResizeManipulator: SVGElement;
    heightResizeManipulator: SVGGElement;
    widthResizeManipulator: SVGGElement;

    constructor(private element: HandleableElement,
                private root: SVGGElement,
                private workspace: FlowWorkspace,
                private handleOptions: HandleOption[]) {}

    init() {
        this.handleGroup = document.createElementNS(SvgNS, 'g');

        this.handleGroup.setAttribute('class', 'manipulators hidden');

        if (this.handleOptions.indexOf('resize_width_height') >= 0) {
            const m = this.widthHeightResizeManipulator = document.createElementNS(SvgNS, 'g');

            const iconBackground = document.createElementNS(SvgNS, 'rect');
            iconBackground.setAttribute('class', 'handle-icon-background');
            iconBackground.setAttributeNS(null, 'rx', '4');
            iconBackground.setAttributeNS(null, 'width', RESIZE_MANIPULATOR_SIZE + '');
            iconBackground.setAttributeNS(null, 'height', RESIZE_MANIPULATOR_SIZE + '');
            m.appendChild(iconBackground);

            const resizeIcon = gen_width_height_resize_icon(RESIZE_MANIPULATOR_SIZE);
            m.appendChild(resizeIcon);
            m.onmousedown = this._startResizeWidthHeight.bind(this);
            m.ontouchstart = this._startResizeWidthHeight.bind(this);

            m.setAttribute('class', 'manipulator resize-manipulator width-height-resizer');
            this.handleGroup.appendChild(m);
        }
        else if (this.handleOptions.indexOf('resize_height') >= 0) {
            this._createHeightResizeManipulator();
        }
        else if (this.handleOptions.indexOf('resize_width') >= 0) {
            this. _createWidthResizeManipulator();
        }

        if (this.handleOptions.indexOf('adjust_settings') >= 0) {
            const m = this.settingsManipulator = document.createElementNS(SvgNS, 'g');

            const iconSettings = document.createElementNS(SvgNS, 'rect');
            iconSettings.setAttribute('class', 'handle-icon-background');
            iconSettings.setAttributeNS(null, 'rx', '4');
            iconSettings.setAttributeNS(null, 'width', SETTINGS_MANIPULATOR_SIZE + '');
            iconSettings.setAttributeNS(null, 'height', SETTINGS_MANIPULATOR_SIZE + '');
            m.appendChild(iconSettings);

            const resizeIcon = gen_settings_manipulator_icon(SETTINGS_MANIPULATOR_SIZE);
            m.appendChild(resizeIcon);
            m.onmousedown = this._startUpdateSettings.bind(this);
            m.ontouchstart = this._startUpdateSettings.bind(this);

            m.setAttribute('class', 'manipulator settings-manipulator');
            this.handleGroup.appendChild(m);
        }

        this.root.appendChild(this.handleGroup); // Avoid the manipulators affecting the element
    }

    show() {
        this.handleGroup.classList.remove('hidden');
        this.update()
    }

    hide() {
        this.handleGroup.classList.add('hidden');
    }

    update() {
        this._reposition();
    }

    private _createWidthResizeManipulator() {
        const m = this.widthResizeManipulator = document.createElementNS(SvgNS, 'g');

        const iconBackground = document.createElementNS(SvgNS, 'rect');
        iconBackground.setAttribute('class', 'handle-icon-background');
        iconBackground.setAttributeNS(null, 'rx', '4');
        iconBackground.setAttributeNS(null, 'width', RESIZE_MANIPULATOR_SIZE + '');
        iconBackground.setAttributeNS(null, 'height', RESIZE_MANIPULATOR_SIZE + '');
        m.appendChild(iconBackground);

        const resizeIcon = gen_width_resize_icon(RESIZE_MANIPULATOR_SIZE);
        m.appendChild(resizeIcon);
        m.onmousedown = this._startResizeWidth.bind(this);
        m.ontouchstart = this._startResizeWidth.bind(this);

        m.setAttribute('class', 'manipulator resize-manipulator width-resizer');
        this.handleGroup.appendChild(m);
    }

    private _createHeightResizeManipulator() {
        const m = this.heightResizeManipulator = document.createElementNS(SvgNS, 'g');

        const iconBackground = document.createElementNS(SvgNS, 'rect');
        iconBackground.setAttribute('class', 'handle-icon-background');
        iconBackground.setAttributeNS(null, 'rx', '4');
        iconBackground.setAttributeNS(null, 'width', RESIZE_MANIPULATOR_SIZE + '');
        iconBackground.setAttributeNS(null, 'height', RESIZE_MANIPULATOR_SIZE + '');
        m.appendChild(iconBackground);

        const resizeIcon = gen_height_resize_icon(RESIZE_MANIPULATOR_SIZE);
        m.appendChild(resizeIcon);
        m.onmousedown = this._startResizeHeight.bind(this);

        m.setAttribute('class', 'manipulator resize-manipulator height-resizer');
        this.handleGroup.appendChild(m);
    }

    // Event handling
    private _startResizeWidthHeight(ev: MouseEvent | TouchEvent) {
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        this.workspace.startResizing(this.element as any as Resizeable, ev);

        return true;
    }

    private _startResizeHeight(ev: MouseEvent | TouchEvent) {
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        // TODO: Add resize type
        this.workspace.startResizing(this.element as any as Resizeable, ev);

        return true;
    }

    private _startResizeWidth(ev: MouseEvent | TouchEvent) {
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        // TODO: Add resize type
        this.workspace.startResizing(this.element as any as Resizeable, ev);

        return true;
    }

    private _startUpdateSettings(ev: MouseEvent | TouchEvent){
        ev.preventDefault();           // Avoid triggering default events
        ev.stopImmediatePropagation(); // Avoid triggering other custom events up-tree

        (this.element as any as ConfigurableSettingsElement).startAdjustingSettings();

        return true;
    }

    private _reposition() {
        const box = this.element.getBlock().getBodyArea();

        if (this.widthHeightResizeManipulator) {
            this.widthHeightResizeManipulator.setAttributeNS(null, 'transform', `translate(${box.width}, ${box.height})`);
        }

        if (this.heightResizeManipulator) {
            this.heightResizeManipulator.setAttributeNS(null, 'transform', `translate(${box.width / 2}, ${box.height + HEIGHT_MANIPULATOR_VERTICAL_PADDING})`);
        }

        if (this.widthResizeManipulator) {
            this.widthResizeManipulator.setAttributeNS(null, 'transform', `translate(${box.width + WIDTH_MANIPULATOR_HORIZONTAL_PADDING}, ${box.height / 2})`);
        }

if (this.settingsManipulator) {
            this.settingsManipulator.setAttributeNS(null, 'transform', `translate(${box.width}, ${0})`);
        }
    }
}
