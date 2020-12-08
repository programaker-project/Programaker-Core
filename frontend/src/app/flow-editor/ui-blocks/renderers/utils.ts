import { Area2D, ManipulableArea2D } from "../../flow_block";


const SvgNS = "http://www.w3.org/2000/svg";

export function getRefBox(canvas: SVGElement): DOMRect {
    const refText = document.createElementNS(SvgNS, 'text');
    refText.setAttribute('class', 'node_name');
    refText.setAttributeNS(null,'textlength', '100%');

    refText.setAttributeNS(null, 'x', "0");
    refText.setAttributeNS(null, 'y', "0");
    refText.textContent = "test";
    canvas.appendChild(refText);

    const refBox = refText.getClientRects()[0];

    canvas.removeChild(refText);

    return refBox;
}

export function combinedManipulableArea(areas: Area2D[]): ManipulableArea2D {
    if (areas.length === 0) {
        return {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
        };
    }

    const initialArea = areas[0];
    let rect = {
        left: initialArea.x,
        top: initialArea.y,
        right: initialArea.x + initialArea.width,
        bottom: initialArea.y + initialArea.height,
    };

    for (let i = 1; i < areas.length; i++) {
        const bArea = areas[i];

        let bRect = {
            left: bArea.x,
            top: bArea.y,
            right: bArea.x + bArea.width,
            bottom: bArea.y + bArea.height,
        };

        rect.left = Math.min(rect.left, bRect.left);
        rect.top = Math.min(rect.top, bRect.top);
        rect.right = Math.max(rect.right, bRect.right);
        rect.bottom = Math.max(rect.bottom, bRect.bottom);
    }

    return rect;
}

function manipulableAreaToArea2D(area: ManipulableArea2D) {
    return {
        x: area.left,
        y: area.top,
        width: area.right - area.left,
        height: area.bottom - area.top,
    };
}

export function combinedArea(areas: Area2D[]): Area2D {
    const combined = combinedManipulableArea(areas);

    return manipulableAreaToArea2D(combined);
}

type FormatType = 'bold' | 'italic' | 'underline';
type TextChunk = { type: 'text', value: string };
type FormatChunk = { type: 'format', format: FormatType, contents: FormattedTextTree }

export type FormattedTextTree = (TextChunk | FormatChunk)[];

function getFormatTypeOfElement(el: HTMLElement): FormatType | null {
    switch (el.tagName.toLowerCase()) {
        case 'b':
        case 'strong':
            return 'bold';

        case 'i':
        case 'em':
            return 'italic';

        case 'u':
            return 'underline';

        default:
            return null;
    }
}

function formatTypeToElement(ft: FormatType): string {
    switch (ft) {
        case 'bold':
            return 'b';

        case 'italic':
            return 'i';

        case 'underline':
            return 'u';

        default:
            return null;
    }
}

export function domToFormattedTextTree(node: Node) : FormattedTextTree {
    if (node instanceof Text) {
        return [{ type: 'text', value: node.textContent }];
    }
    else if (node instanceof HTMLElement) {
        const formatType = getFormatTypeOfElement(node);
        let  subTrees = [];

        for (const n of Array.from(node.childNodes)){
            subTrees = subTrees.concat(domToFormattedTextTree(n));
        }

        if (!formatType) {
            return subTrees;
        }
        else {
            return [{ type: 'format', format: formatType, contents: subTrees }];
        }
    }
    else {
        throw Error("Unexpected node type: " + node);
    }
}

export function formattedTextTreeToDom(tt: FormattedTextTree): Node {
    const nodes = [];

    for (const el of tt) {
        if (el.type === 'text') {
            nodes.push(document.createTextNode(el.value));
        }
        else if (el.type === 'format') {
            const node = document.createElement(formatTypeToElement(el.format));

            node.appendChild(formattedTextTreeToDom(el.contents));

            nodes.push(node);
        }
    }

    if (nodes.length === 1) {
        return nodes[0];
    }

    const wrapper = document.createElement('div');
    for (const node of nodes){
        wrapper.appendChild(node);
    }

    return wrapper;
}

export function startOnElementEditor(element: HTMLDivElement, parent: SVGForeignObjectElement, onDone: (text: FormattedTextTree) => void) {
    const elementPos = element.getClientRects()[0];

    const buttonBar = document.createElement('div');
    buttonBar.classList.add('floating-button-bar');

    const boldButton = document.createElement('button');
    boldButton.classList.add('bold-button');
    buttonBar.appendChild(boldButton);
    boldButton.innerText = 'B';
    boldButton.onmousedown = (ev) => {
        document.execCommand('bold', false, undefined);
        ev.preventDefault(); // Prevent losing focus on element
    }

    const italicButton = document.createElement('button');
    italicButton.classList.add('italic-button');
    buttonBar.appendChild(italicButton);
    italicButton.innerText = 'I';
    italicButton.onmousedown = (ev) => {
        document.execCommand('italic', false, undefined);
        ev.preventDefault(); // Prevent losing focus on element
    }

    const underlineButton = document.createElement('button');
    underlineButton.classList.add('underline-button');
    buttonBar.appendChild(underlineButton);
    underlineButton.innerText = 'U';
    underlineButton.onmousedown = (ev) => {
        document.execCommand('underline', false, undefined);
        ev.preventDefault(); // Prevent losing focus on element
    }

    document.body.appendChild(buttonBar);

    const buttonDim = buttonBar.getClientRects()[0];
    buttonBar.style.top = elementPos.y - buttonDim.height + 'px';
    buttonBar.style.left = elementPos.x + 'px';

    element.oninput = () => {
    }

    element.onkeydown = (ev: KeyboardEvent) => {
        if (ev.ctrlKey && ev.code === 'KeyB') {
            ev.preventDefault();
            document.execCommand('bold', false, undefined);
        }
        else if (ev.ctrlKey && ev.code === 'KeyU') {
            ev.preventDefault();
            document.execCommand('underline', false, undefined);
        }
        else if (ev.ctrlKey && ev.code === 'KeyI') {
            ev.preventDefault();
            document.execCommand('italic', false, undefined);
        }
        else if (ev.ctrlKey && ev.code === 'Enter') {
            ev.preventDefault();
            element.blur(); // Release focus
        }
    }

    element.onblur = (ev) => {
        // Cleanup
        element.onkeydown = element.onblur = element.oninput = null;
        document.body.removeChild(buttonBar);

        onDone(domToFormattedTextTree(element));
    }
}
