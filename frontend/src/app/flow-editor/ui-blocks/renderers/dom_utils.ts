import { UnderlineSettings } from "../../dialogs/configure-link-dialog/configure-link-dialog.component";

export function isTagOnAncestors(node: Node, tag: string): null | {tags: string[], ancestor: HTMLElement} {
    if (!(node instanceof HTMLElement)) {
        return isTagOnAncestors(node.parentElement, tag);
    }

    let element = node as HTMLElement;
    const tags = [];

    while (element) {
        if (element.tagName.toLowerCase() === 'foreignobject') {
            return null;
        }
        else if (element.tagName.toLowerCase() === tag) {
            tags.reverse();
            return {
                tags,
                ancestor: element,
            };
        }
        else {
            tags.push(element.tagName.toLowerCase());
            element = element.parentElement;
        }
    }

    return null;
}

export function isTagOnTree(node: Node, tag: string):  null | HTMLElement {
    if (node instanceof HTMLElement) {
        if (node.tagName.toLowerCase() === tag) {
            return node;
        }
    }

    for (const child of Array.from(node.childNodes)) {
        const inChild = isTagOnTree(child, tag);
        if (inChild) {
            return inChild;
        }
    }

    return null;
}


export function extractContentsToRight(element: HTMLElement) {
    const parent = element.parentNode;
    const next = element.nextSibling;
    for (const node of Array.from(element.childNodes)) {
        if (next) {
            parent.insertBefore(node, next);
        }
        else {
            parent.appendChild(node);
        }
    }
    parent.removeChild(element);
}

export function surroundRangeWithElement(range: Range, element: HTMLElement) {

    // The difference with Range.surroundContents() is that this supports
    // ranges that start at one tag and end at another.
    // In exchange it has to clone whole tags, not supporting partial ones.


    const contents = range.extractContents();
    element.appendChild(contents);
    range.insertNode(element);
}


// Taken from: https://stackoverflow.com/a/3627747
export function colorToHex(rgb: string): string {
    if (/^#[0-9A-F]{3,6}$/i.test(rgb)) {
        return rgb;
    }

    const match = rgb.match(/^rgb\((\d+),\s*(\d+),\s*(\d+)\)$/);
    if (!match) {
        return null;
    }
    function hex(x) {
        return ("0" + parseInt(x).toString(16)).slice(-2);
    }
    return "#" + hex(match[1]) + hex(match[2]) + hex(match[3]);
}

export function getUnderlineSettings(tag: HTMLAnchorElement): UnderlineSettings {
    if (tag.style.textDecoration === 'none') {
        return 'none';
    }
    else if (tag.style.textDecorationColor && tag.style.textDecorationColor != 'revert') {
        const hex = colorToHex(tag.style.textDecorationColor);
        if (!hex) {
            console.warn("Error parsing underline color", tag.style.textDecorationColor);
        }
        return { color: hex || '#000000' };
    }
    else {
        return 'default';
    }
}

export function applyUnderlineSettings(tag: HTMLAnchorElement, underline: UnderlineSettings) {
    if ((underline === 'default') || (!underline)) {
        tag.style.textDecoration = 'revert';
        tag.style.textDecorationColor = 'revert';
    }
    else if (underline === 'none') {
        tag.style.textDecoration = 'none';
    }
    else {
        tag.style.textDecoration = 'revert';
        tag.style.textDecorationColor = underline.color;
    }
}

function flattenTag(element: Node) {
    const parent = element.parentNode;
    const next = element.nextSibling;
    for (const node of Array.from(element.childNodes)) {
        if (next) {
            parent.insertBefore(node, next);
        }
        else {
            parent.appendChild(node);
        }
    }
    parent.removeChild(element);
}

export function flattenAllTagsUnder(root: HTMLElement, tagNameToFlatten: string) {
    const toFlatten = root.querySelectorAll(tagNameToFlatten);
    for (const tag of Array.from(toFlatten)) {
        flattenTag(tag);
    }
}
