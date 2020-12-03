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
