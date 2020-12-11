import { MatDialog } from "@angular/material/dialog";
import { ConfigureFontColorDialogComponent } from "../../dialogs/configure-font-color-dialog/configure-font-color-dialog.component";
import { ConfigureLinkDialogComponent, UnderlineSettings } from "../../dialogs/configure-link-dialog/configure-link-dialog.component";
import { Area2D, ManipulableArea2D } from "../../flow_block";
import { extractContentsToRight, isTagOnAncestors, isTagOnTree, surroundRangeWithElement, getUnderlineSettings, applyUnderlineSettings, colorToHex } from "./dom_utils";


const SvgNS = "http://www.w3.org/2000/svg";

const DEFAULT_FONT_COLOR = '#000000';

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
type TextColorChunk = { type: 'text-color', color: string, contents: FormattedTextTree };
type LinkChunk = { type: 'link', target: string, open_in_tab: boolean, underline: UnderlineSettings, contents: FormattedTextTree };
type FormatChunk = { type: 'format', format: FormatType, contents: FormattedTextTree }

export type FormattedTextTree = (TextChunk | FormatChunk | LinkChunk | TextColorChunk)[];

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

function trimFormattedTextTree(tree: FormattedTextTree): FormattedTextTree {
    // Note that this is a destructive operation, the inputted text tree will be the same as the outputted one
    while (tree.length > 0) {
        const last = tree[tree.length - 1];

        // If the last element is a whitespace, remove it
        if (last.type === 'text' && last.value.trim() === '') {
            tree.pop();
        }
        else {
            break;
        }
    }

    return tree;
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

        if (node instanceof HTMLDivElement) {
            subTrees.push({ type: 'text', value: '\n' });
        }

        if (node instanceof HTMLAnchorElement) {
            const openInTab = node.target && (node.target.indexOf('_blank') >= 0);
            return [{
                type: 'link',
                target: node.href,
                open_in_tab: !!openInTab,
                underline: getUnderlineSettings(node),
                contents: subTrees,
            }];
        }

        if (node instanceof HTMLFontElement) {
            return [{ type: 'text-color', color: node.style.color, contents: subTrees }];
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

function unwrapSpan(node: Node): Node[] {
    if (node instanceof HTMLSpanElement) {
        let elements = [];
        for (const e of Array.from(node.childNodes)) {
            const unwrapped = unwrapSpan(e);
            if (unwrapped.length === 0) {
                // Nothing to do
            }
            else if (unwrapped.length === 1){
                // No need to create a new list
                elements.push(unwrapped[0]);
            }
            else {
                elements = elements.concat(unwrapped);
            }
        }

        return elements;
    }
    else {
        return [node];
    }
}

export function formattedTextTreeToDom(tt: FormattedTextTree, nested?: boolean): Node {
    const nodes = [];

    let group = document.createElement(nested ? 'span' : 'div');
    nodes.push(group);

    for (const el of tt) {
        if (el.type === 'text') {
            if (el.value === '\n') {
                if (group && (group.tagName.toLowerCase() === 'div') && group.innerText == '') {
                    group.innerHTML = '&nbsp';
                }

                group = document.createElement('div');
                nodes.push(group);
            }
            else {
                const node = document.createTextNode(el.value);

                group.appendChild(node);
            }
        }
        else if (el.type === 'format') {
            const node = document.createElement(formatTypeToElement(el.format));

            const contents = formattedTextTreeToDom(el.contents, true);

            node.append(...unwrapSpan(contents));

            group.appendChild(node);
        }
        else if (el.type === 'link') {
            const node = document.createElement('a');

            node.href = el.target;
            if (el.open_in_tab) {
                node.target = '_blank';
            }
            applyUnderlineSettings(node, el.underline);
            const contents = formattedTextTreeToDom(el.contents, true);
            node.append(...unwrapSpan(contents));

            group.appendChild(node);
        }
        else if (el.type === 'text-color') {
            const node = document.createElement('font');

            node.style.color = el.color;
            const contents = formattedTextTreeToDom(el.contents, true);
            node.append(...unwrapSpan(contents));

            group.appendChild(node);
        }
    }

    if (group && (group.tagName.toLowerCase() === 'div') && group.innerText == '') {
        group.innerHTML = '&nbsp';
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

function editColorInSelection(dialog: MatDialog): Promise<void> {
    return new Promise<void>((resolve, reject) => {
        const sel = window.getSelection();
        const range = sel.getRangeAt(0);
        const contents = range.cloneContents();
        let text = contents.textContent;
        let fontTag: HTMLFontElement | null = null;

        const ancestorInfo = isTagOnAncestors(range.commonAncestorContainer, 'font');
        if (ancestorInfo) {
            fontTag = ancestorInfo.ancestor as HTMLFontElement;
        }
        else  {
            fontTag = isTagOnTree(contents, 'a') as HTMLFontElement;
        }

        const newWrapper = !fontTag;
        if (fontTag) {
            text = fontTag.textContent;
        }
        else {
            fontTag = document.createElement('font');
            fontTag.style.color = DEFAULT_FONT_COLOR;
            surroundRangeWithElement(range, fontTag);
        }

        const dialogRef = dialog.open(ConfigureFontColorDialogComponent, {
            data: { text: text, color: colorToHex(fontTag.style.color ? fontTag.style.color : DEFAULT_FONT_COLOR) }
        });

        dialogRef.afterClosed().subscribe(async (result) => {
            if (!(result && result.success)) {
                if (newWrapper) {
                    // Unwrap elements
                    extractContentsToRight(fontTag);
                }
            }
            else if (result.operation === 'remove-color') {
                extractContentsToRight(fontTag);
            }
            else {
                // Update the <a> tag with the appropriate link
                fontTag.style.color = result.value.color;
            }
            resolve();
        });
    });
}

function editLinkInSelection(dialog: MatDialog): Promise<void> {
    return new Promise<void>((resolve, reject) => {
        const sel = window.getSelection();
        const range = sel.getRangeAt(0);
        const contents = range.cloneContents();
        let text = contents.textContent;
        let linkValue = '';
        let linkTag: HTMLAnchorElement | null = null;

        const ancestorInfo = isTagOnAncestors(range.commonAncestorContainer, 'a');
        if (ancestorInfo) {
            linkTag = ancestorInfo.ancestor as HTMLAnchorElement;
        }
        else  {
            linkTag = isTagOnTree(contents, 'a') as HTMLAnchorElement;
        }

        const newWrapper = !linkTag;
        if (linkTag) {
            linkValue = linkTag.href;
            text = linkTag.textContent;
        }
        else {
            linkTag = document.createElement('a');
            surroundRangeWithElement(range, linkTag);
        }

        const openInTab = linkTag.target && linkTag.target.indexOf('_blank') >= 0;

        const underline: UnderlineSettings = getUnderlineSettings(linkTag);

        const dialogRef = dialog.open(ConfigureLinkDialogComponent, {
            data: { text: text, link: linkValue, openInTab: openInTab, underline: underline }
        });

        dialogRef.afterClosed().subscribe(async (result) => {
            if (!(result && result.success)) {
                if (newWrapper) {
                    // Unwrap elements
                    extractContentsToRight(linkTag);
                }
            }
            else if (result.operation === 'remove-link') {
                extractContentsToRight(linkTag);
            }
            else {
                // Update the <a> tag with the appropriate link
                linkTag.href = result.value.link;
                linkTag.innerText = result.value.text;

                if (result.value.openInTab) {
                    linkTag.target = '_blank';
                }
                else {
                    linkTag.target = '';
                }

                applyUnderlineSettings(linkTag, result.value.underline);
            }

            resolve();
        });
    });
}

export function startOnElementEditor(element: HTMLDivElement, parent: SVGForeignObjectElement, dialog: MatDialog, onDone: (text: FormattedTextTree) => void) {
    const elementPos = element.getClientRects()[0];

    const buttonBar = document.createElement('div');
    {
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

        const colorButton = document.createElement('button');
        colorButton.classList.add('color-button');
        buttonBar.appendChild(colorButton);
        colorButton.innerHTML = '<img class="icon" src="/assets/icons/format_color_text.svg" />';
        colorButton.onmousedown = (ev) => {
            ev.preventDefault(); // Prevent losing focus on element
            withMovingFocus(() => editColorInSelection(dialog));
        }

        const linkButton = document.createElement('button');
        linkButton.classList.add('link-button');
        buttonBar.appendChild(linkButton);
        linkButton.innerHTML = '<img class="icon" src="/assets/icons/insert_link.svg" />';
        linkButton.onmousedown = (ev) => {
            ev.preventDefault(); // Prevent losing focus on element
            withMovingFocus(() => editLinkInSelection(dialog));
        }

        document.body.appendChild(buttonBar);

        const buttonDim = buttonBar.getClientRects()[0];
        buttonBar.style.top = elementPos.y - buttonDim.height + 'px';
        buttonBar.style.left = elementPos.x + 'px';
    }


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
        else if (ev.ctrlKey && ev.code === 'KeyK') {
            ev.preventDefault();
            withMovingFocus(() => editLinkInSelection(dialog));
        }
        else if (ev.ctrlKey && ev.code === 'Enter') {
            ev.preventDefault();
            element.blur(); // Release focus
        }
    }

    const onBlur = (_ev: FocusEvent) => {
        // Cleanup
        element.onkeydown = element.onblur = element.oninput = null;
        document.body.removeChild(buttonBar);

        onDone(trimFormattedTextTree(domToFormattedTextTree(element)));
    };

    const withMovingFocus = (f: () => Promise<void> ) => {
        element.onblur = null;
        const wasFocus = element.onfocus;
        element.onfocus = null;
        f().then(() => {
            element.focus();
            element.onfocus = wasFocus;
            element.onblur = onBlur;
        });
    };

    element.onblur = onBlur;
}
