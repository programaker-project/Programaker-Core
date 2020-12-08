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

export function startOnElementEditor(element: HTMLDivElement, parent: SVGForeignObjectElement, onDone: () => void) {
    const isTagOnAncestors = (node: Node, tag: string): {tags: string[], ancestor: HTMLElement} => {
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

    const isTagOnTree = (node: Node, tag: string): boolean => {
        if (node instanceof HTMLElement) {
            if (node.tagName.toLowerCase() === tag) {
                return true;
            }
        }

        for (const child of Array.from(node.childNodes)) {
            if (isTagOnTree(child, tag)) {
                return true;
            }
        }

        return false;
    }

    const cutToRight = (start: Node, ancestor: HTMLElement, acc?: HTMLElement) => {
        const parNode = start.parentNode;
        console.log("ParNode", parNode);
        const parent = start.parentElement;

        const cut = document.createElement(parent.tagName);

        let it = start;
        if (acc) {
            cut.appendChild(acc);
            it = start.nextSibling;
        }

        console.log(it);
        while (it) {
            const next = it.nextSibling;
            cut.appendChild(it);
            it = next;
            console.log(it);
        }

        if (parent === ancestor) {
            return cut;
        }

        console.log("Up", parNode);

        return cutToRight(parNode, ancestor, cut);
    }

    const wrapInTags = (node: Node, tags: string[]): Node => {
        if (tags.length === 0) {
            return node;
        }
        const top = document.createElement(tags[0]);
        let lower = top;

        for (let i = 1; i < tags.length; i++) {
            const next = document.createElement(tags[i]);
            lower.appendChild(next);
            lower = next;
        }

        lower.appendChild(node);

        return top;
    }

    const isAllTreeOnTag = (node: Node, tag: string): boolean => {
        if (node instanceof HTMLElement) {
            if (node.tagName.toLowerCase() === tag) {
                return true;
            }
            else {
                // If any child inside the node is not on the tag, not all tree is in it.
                if (Array.from(node.childNodes).some(n => !isAllTreeOnTag(n, tag))) {
                    return false;
                }
                return true;
            }
        }
        else if (node instanceof Text) {
            return node.textContent.trim() === ''; // Ignore whitespace nodes
        }
        else if (node instanceof DocumentFragment) {
            // Same logic as tags
            // If any child inside the node is not on the tag, not all tree is in it.
            if (Array.from(node.childNodes).some(n => !isAllTreeOnTag(n, tag))) {
                return false;
            }
            return true;
        }
        else {
            console.log('Problematic node:', node);
            throw Error(`Unexpected node: ${node}`);
        }
    };

    const removeTag = (node: DocumentFragment, tag: string): Node => {
        const toExtract = node.querySelectorAll(tag);

        for (const e of Array.from(toExtract)) {
            let lastNode = e;
            for (const node of Array.from(e.childNodes)) {
                lastNode.after(node);
                lastNode = node as Element;
            }
            e.parentNode.removeChild(e);
        }

        return node;
    }

    const toggleTagOnSelection = (tag: string) => {
        const sel = window.getSelection();
        if (!sel) {
            return;
        }

        const range = sel.getRangeAt(0).cloneRange();
        console.log("Selected", range);

        const ancestorTags = isTagOnAncestors(range.commonAncestorContainer, tag);
        const selectedContents = range.cloneContents();

        if (ancestorTags) {
            // Rip selection out of others in the same tag
            range.insertNode(range.extractContents());

            console.log("In ancestor");
            // Move elements on the right of the selection to a structure like the existing one
            const nextElement = range.endContainer.nextSibling;
            console.log("NXT", nextElement);
            const toRight = cutToRight(nextElement, ancestorTags.ancestor);
            console.log("ToRight", toRight);
            ancestorTags.ancestor.after(toRight);

            // Replicate tree without the toggled tag on the selection
            const reTagged = wrapInTags(range.extractContents(), ancestorTags.tags);
            ancestorTags.ancestor.after(reTagged);
        }
        else if (isTagOnTree(selectedContents, tag)) {
            console.log("On tree");

            const contents = range.extractContents();
            const toExtract = contents.querySelectorAll(tag);
            console.log("ToExtract", toExtract);

            for (const e of Array.from(toExtract)) {
                let lastNode = e;
                for (const node of Array.from(e.childNodes)) {
                    lastNode.after(node);
                    lastNode = node as Element;
                }
                e.parentNode.removeChild(e);
            }

            console.log("Contents:", contents);

            const allTreeOnTag = isAllTreeOnTag(selectedContents, tag);
            console.log("AllTreeOnTag:", allTreeOnTag);

            if (allTreeOnTag) {
                range.insertNode(removeTag(selectedContents, tag));
            }
            else {
                const wrapper = document.createElement(tag);
                wrapper.appendChild(contents);
                range.insertNode(wrapper)
            }
        }
        else {
            console.log("Surround");

            const wrapper = document.createElement(tag);
            range.surroundContents(wrapper);
        }

    };

    // TODO: Add buttons

    element.oninput = () => {
    }

    element.onkeydown = (ev: KeyboardEvent) => {
        if (ev.ctrlKey && ev.code === 'KeyB') {
            ev.preventDefault();
            toggleTagOnSelection('strong');
        }
        else if (ev.ctrlKey && ev.code === 'KeyU') {
            ev.preventDefault();
            toggleTagOnSelection('u');
        }
        else if (ev.ctrlKey && ev.code === 'KeyI') {
            ev.preventDefault();
            toggleTagOnSelection('em');
        }
        else if (ev.ctrlKey && ev.code === 'Enter') {
            ev.preventDefault();
            element.blur(); // Lose focus
        }
    }

    element.onblur = (ev) => {
        element.onkeydown = element.onblur = element.oninput = null;
        onDone();
    }
}
