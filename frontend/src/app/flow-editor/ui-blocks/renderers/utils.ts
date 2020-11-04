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
