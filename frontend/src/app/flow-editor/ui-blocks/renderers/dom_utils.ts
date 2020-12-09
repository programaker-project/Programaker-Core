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
