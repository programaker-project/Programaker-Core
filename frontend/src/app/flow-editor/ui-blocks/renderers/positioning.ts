import { Area2D, FlowBlock, Position2D } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockHandler } from "../ui_flow_block";
import { cleanestTree, getElementsInGroup, getRect, getShallowElementsInGroup, safeReduceTree } from "./responsive_page";
import { CutNode, CutTree, ContainerElementRepr, CutType, DEFAULT_CUT_TYPE } from "./ui_tree_repr";
import { manipulableAreaToArea2D } from "./utils";
import { ContainerFlowBlock } from "../container_flow_block";

export const SEPARATION = 25;

interface ResponsivePositionToCenter {
    cut_type: CutType,
    elements: UiFlowBlock[],
    treeElements: UiFlowBlock[],
};

// Positioning
export function PositionResponsiveContents(handler: UiFlowBlockHandler,
                                           blocks: FlowBlock[],
                                           allBlocks: FlowBlock[],
                                           offset: Position2D,
                                          ): {tree: CutTree | null, toCenter: ResponsivePositionToCenter[]} {
    // Format in a grid-like
    const uiPos = (blocks
        .filter(b => (b instanceof UiFlowBlock))
        .map((b: UiFlowBlock, i) => {
            const area = b.getBodyArea()
            if (b.isAutoresizable()) {
                const min = b.getMinSize();
                area.width = min.width;
                area.height = min.height;
            }

            return {i, a: area, b: (b as UiFlowBlock)};
        }));

    if (uiPos.length === 0){
        return { tree: null, toCenter: [] };
    }

    const tree = safeReduceTree(cleanestTree(uiPos, uiPos.map(({b: block}) => block)));

    const uiBlocks = allBlocks.filter(b => b instanceof UiFlowBlock) as UiFlowBlock[];
    const blockMap: {[key: string]: UiFlowBlock} = {};
    for (const block of uiBlocks) {
        blockMap[block.id] = block;
    }

    let positioningTree = tree;

    // Force to at least center vertically
    if (!(positioningTree as CutNode).cut_type) {
        positioningTree = { cut_type: DEFAULT_CUT_TYPE, groups: [ positioningTree ] };
    }

    const toCenter = PositionTreeContentsFromTree(positioningTree, blockMap, offset);

    return { tree, toCenter };
}

function PositionTreeContentsFromTree(tree: CutTree, blocks: {[key: string]: UiFlowBlock}, offset: Position2D, nonTopLevel?: boolean): ResponsivePositionToCenter[] {
    if (!(tree as CutNode).cut_type) {
        return [];
    }

    if ((tree as CutNode).block_id) {
        const id = (tree as CutNode).block_id;
        const block = blocks[id];

        if (block instanceof ContainerFlowBlock) {
            // The block should have repositioned itself, no need to call it again
        }
        else {
            throw Error("Cut with blockId that didn't correspond to a ContainerFlowBlock");
        }
        return [];
    }

    let positions = [];

    const cTree = tree as CutNode;
    const toCenter: UiFlowBlock[] = [];

    // First position subtrees
    const subTreeOffset: Position2D = {
        x: offset.x + (nonTopLevel ? 0 : SEPARATION),
        y: offset.y + (nonTopLevel ? 0 : SEPARATION),
    };

    for (const group of cTree.groups) {
        let area: Area2D;

        if (((group as CutNode).block_id) || ((group as ContainerElementRepr).container_type) ) {
            // Treat as a single block

            const id = (group as CutNode).block_id ? (group as CutNode).block_id : (group as ContainerElementRepr).id ;
            const block = blocks[id];
            area = block.getBodyArea();

            const mov = {
                x: subTreeOffset.x - area.x,
                y: subTreeOffset.y - area.y,
            };

            if (block.isAutoresizable()) {
                // Don't push away from borders
                if (cTree.cut_type === 'vbox') {
                    if (block.doesTakeAllHorizontal()) {
                        mov.x = 0;
                    }
                }
            }

            block.moveBy(mov);
            toCenter.push(block);

            area = block.getBodyArea();
            if (block.isAutoresizable) {
                const minSize = block.getMinSize();
                area.height = Math.max(minSize.height, area.height);
                area.width = Math.max(minSize.width, area.width);
            }
        }
        else {
            // Treat as a group
            const subOffset = { x: subTreeOffset.x, y: subTreeOffset.y };

            positions = positions.concat(PositionTreeContentsFromTree(group, blocks, subOffset, true));

            const contents = getElementsInGroup(group);
            area = manipulableAreaToArea2D(getRect(contents.map(id => blocks[id])));
            const mov = {
                x: subTreeOffset.x - area.x,
                y: subTreeOffset.y - area.y,
            };

            const elementsToMove = getShallowElementsInGroup(group).map(id => blocks[id]);

            for (const block of elementsToMove) {

                if (block.isAutoresizable()) {

                    // This really means "is this considered on PositionTreeContentsFromTree()"
                    if (block instanceof ContainerFlowBlock) {
                        continue;
                    }

                    // Don't push away from borders
                    if (cTree.cut_type === 'vbox') {
                        mov.x -= SEPARATION;
                    }
                    else if (cTree.cut_type === 'hbox') {
                        mov.y -= SEPARATION;
                    }
                }
                else {
                    toCenter.push(block);
                }

                block.moveBy(mov);
            }

            area = manipulableAreaToArea2D(getRect(contents.map(id => blocks[id])));
        }

        if (cTree.cut_type === 'vbox') {
            subTreeOffset.y += area.height + SEPARATION;
        }
        else if (cTree.cut_type === 'hbox') {
            subTreeOffset.x += area.width + SEPARATION;
        }
    }

    positions.push({ cut_type: cTree.cut_type, elements: toCenter, treeElements: getElementsInGroup(cTree).map(id => blocks[id]) });
    return positions;

}

export function PositionHorizontalContents(handler: UiFlowBlockHandler, blocks: FlowBlock[], area: Area2D): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => {
                const area = b.getBodyArea()
                let minArea: Area2D;
                if (b.isAutoresizable()) {
                    const min = b.getMinSize();
                    area.width = min.width;
                    area.height = min.height;
                    minArea = area;
                }
                else {
                    minArea = Object.assign({}, area);
                    area.height += SEPARATION * 2;
                }

                return [b, area, minArea];
            }));

    const blockHeights = blockAreas.map(([_, a]) => a.height);
    const maxHeight = Math.max(...blockHeights);

    const blockWidths = blockAreas.map(([_, a]) => a.width);
    const reqBlockWidth = blockWidths.reduce((a,b) => a + b, 0);

    const sumPaddings = SEPARATION * (blocks.length + 1);
    const reqWidth = reqBlockWidth + sumPaddings;
    const height = maxHeight;

    const separation = SEPARATION;

    blockAreas.sort(([_11, a1, _13], [_21, a2, _23]) => a1.x - a2.x);
    let xpos = separation;
    for (const [block, blockArea, minArea] of blockAreas) {
        const x = xpos;
        const y = block.isAutoresizable() ? 0 : (height - minArea.height) / 2;

        const absX = area.x + x;
        const absY = area.y + y;

        block.moveBy({
            x: absX - blockArea.x,
            y: absY - blockArea.y,
        });

        const areaAfterMove = block.getBodyArea()
        if (block.isAutoresizable()) {
            const min = block.getMinSize();
            areaAfterMove.width = min.width;
            areaAfterMove.height = min.height;
        }

        xpos += areaAfterMove.width + separation;
    }

    return { width: xpos, height };
}

export function PositionVerticalContents(handler: UiFlowBlockHandler, blocks: FlowBlock[], area: Area2D): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => {
                const area = b.getBodyArea();
                let minArea: Area2D;
                if (b.isAutoresizable()) {
                    const min = b.getMinSize();
                    area.width = min.width;
                    area.height = min.height;
                    minArea = area;
                }
                else {
                    minArea = Object.assign({}, area);
                    area.width += SEPARATION * 2;
                }

                return [b, area, minArea];
            }));

    const blockWidths = blockAreas.map(([_, a]) => a.width);
    const maxWidth = Math.max(...blockWidths);

    const blockHeights = blockAreas.map(([_, a]) => a.height);
    const reqBlockHeight = blockHeights.reduce((a,b) => a + b, 0);

    const sumPaddings = SEPARATION * (blocks.length + 1);
    const reqHeight = reqBlockHeight + sumPaddings;
    const width = maxWidth;

    const separation = SEPARATION;

    blockAreas.sort(([_11, a1, _13], [_21, a2, _23]) => a1.y - a2.y);
    let ypos = separation;
    for (const [block, blockArea, minArea] of blockAreas) {
        const y = ypos;
        const x = block.isAutoresizable() && block.doesTakeAllHorizontal() ? 0 : (width - minArea.width) / 2;

        const absX = area.x + x;
        const absY = area.y + y;

        block.moveBy({
            x: absX - blockArea.x,
            y: absY - blockArea.y,
        });

        const areaAfterMove = block.getBodyArea()
        if (block.isAutoresizable()) {
            const min = block.getMinSize();
            areaAfterMove.width = min.width;
            areaAfterMove.height = min.height;
        }

        ypos += areaAfterMove.height + separation;
    }

    return { width, height: ypos };
}

// Sizing
export function GetMinSizeHorizontal(blocks: FlowBlock[]): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => {
                const area = b.getBodyArea()
                if (b.isAutoresizable()) {
                    const min = b.getMinSize();
                    area.width = min.width;
                    area.height = min.height;
                }
                else {
                    area.height += SEPARATION * 2;
                }

                return [b, area];
            }));

    const blockHeights = blockAreas.map(([_, a]) => a.height);
    const maxHeight = Math.max(...blockHeights);

    const blockWidths = blockAreas.map(([_, a]) => a.width);
    const reqBlockWidth = blockWidths.reduce((a,b) => a + b, 0);

    const sumPaddings = SEPARATION * (blocks.length + 1);
    const reqWidth = reqBlockWidth + sumPaddings;

    return { width: reqWidth, height: maxHeight };
}

export function GetMinSizeVertical(blocks: FlowBlock[]): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => {
                const area = b.getBodyArea()
                if (b.isAutoresizable()) {
                    const min = b.getMinSize();
                    area.width = min.width;
                    area.height = min.height;
                }
                else {
                    area.width += SEPARATION * 2;
                }

                return [b, area];
            }));

    const blockWidths = blockAreas.map(([_, a]) => a.width);
    const blockHeights = blockAreas.map(([_, a]) => a.height);

    const maxWidth = Math.max(...blockWidths);
    const reqBlockHeight = blockHeights.reduce((a,b) => a + b, 0);

    const sumPaddings = SEPARATION * (blocks.length + 1);
    const reqHeight = reqBlockHeight + sumPaddings;

    return { width: maxWidth, height: reqHeight };
}

// Centering
export function CenterElements(groups: ResponsivePositionToCenter[]) {
    for (const group of groups) {
        const fullArea = manipulableAreaToArea2D(getRect(group.treeElements));
        for (const elem of group.elements) {
            const eArea = elem.getBodyArea();


            if (elem.isAutoresizable()) {
                const minArea = elem.getMinSize();

                eArea.width = minArea.width;
                eArea.height = minArea.height;
            }

            const mov = { x: 0, y: 0 };
            if (group.cut_type === 'vbox') {
                const xPos = fullArea.x + ((fullArea.width - eArea.width) / 2)
                mov.x = xPos - eArea.x;
            }
            else if (group.cut_type === 'hbox') {
                const yPos = fullArea.y + ((fullArea.height - eArea.height) / 2)
                mov.y = yPos - eArea.y;
            }

            elem.moveBy(mov);
        }
    }
}
