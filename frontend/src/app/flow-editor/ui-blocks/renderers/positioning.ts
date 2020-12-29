import { Area2D, FlowBlock, Position2D } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockHandler } from "../ui_flow_block";
import { cleanestTree, getElementsInGroup, getRect, getShallowElementsInGroup } from "./responsive_page";
import { CutNode, CutTree } from "./ui_tree_repr";
import { manipulableAreaToArea2D } from "./utils";
import { ContainerFlowBlock } from "../container_flow_block";

export const SEPARATION = 10;

export function PositionResponsiveContents (handler: UiFlowBlockHandler, blocks: FlowBlock[], allBlocks: FlowBlock[], offset: Position2D): Area2D {
    // Format in a grid-like
    const uiPos = (blocks
        .filter(b => (b instanceof UiFlowBlock))
        .map((b, i) => {
            return {i, a: b.getBodyArea(), b: (b as UiFlowBlock)};
        }));

    const tree = cleanestTree(uiPos, uiPos.map(({b: block}) => block));

    const uiBlocks = allBlocks.filter(b => b instanceof UiFlowBlock) as UiFlowBlock[];
    const blockMap: {[key: string]: UiFlowBlock} = {};
    for (const block of uiBlocks) {
        blockMap[block.id] = block;
    }

    console.log('RESP Tree', tree);


    return PositionTreeContentsFromTree(tree, blockMap, offset);
}

const SEPARATION = 10;

export function PositionResponsiveContents (handler: UiFlowBlockHandler, blocks: FlowBlock[]) {
    console.log("Resp", handler, blocks);
}

export function PositionHorizontalContents (handler: UiFlowBlockHandler, blocks: FlowBlock[], area: Area2D): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => [b, b.getBodyArea()]));

    const blockWidths = blockAreas.map(([_, a]) => a.width);
    const blockHeights = blockAreas.map(([_, a]) => a.height);

    const maxHeight = Math.max(...blockHeights);
    const reqBlockWidth = blockWidths.reduce((a,b) => a + b, 0);

    const sumPaddings = SEPARATION * (blocks.length + 1);
    const reqWidth = reqBlockWidth + sumPaddings;
    const width = Math.max(area.width, reqWidth);
    const height = maxHeight + SEPARATION * 2;

    const separation = width === reqWidth
        ? SEPARATION
        : (area.width - reqBlockWidth) / (blockWidths.length + 1)
    ;

    blockAreas.sort(([_1, a1], [_2, a2]) => a1.x - a2.x);
    let xpos = separation;
    for (const [block, blockArea] of blockAreas) {
        const x = xpos;
        const y = block.isAutoresizable() ? 0 : (height - blockArea.height) / 2;
        xpos += blockArea.width + separation;

        console.log(block.isAutoresizable(), block.options.id);

        const absX = area.x + x;
        const absY = area.y + y;

        block.moveBy({
            x: absX - blockArea.x,
            y: absY - blockArea.y,
        });
    }

    return { width, height };
}

export function PositionVerticalContents (handler: UiFlowBlockHandler, blocks: FlowBlock[], area: Area2D): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => [b, b.getBodyArea()]));

    const blockWidths = blockAreas.map(([_, a]) => a.width);
    const blockHeights = blockAreas.map(([_, a]) => a.height);

    const maxWidth = Math.max(...blockWidths);
    const reqBlockHeight = blockHeights.reduce((a,b) => a + b, 0);

    const sumPaddings = SEPARATION * (blocks.length + 1);
    const reqHeight = reqBlockHeight + sumPaddings;
    const height = Math.max(area.width, reqHeight);
    const width = maxWidth + SEPARATION * 2;

    const separation = height === reqHeight
        ? SEPARATION
        : (area.height - reqBlockHeight) / (blockHeights.length + 1)
    ;

    blockAreas.sort(([_1, a1], [_2, a2]) => a1.y - a2.y);
    let ypos = separation;
    for (const [block, blockArea] of blockAreas) {
        const y = ypos;
        const x = block.isAutoresizable() ? 0 : (width - blockArea.width) / 2;
        ypos += blockArea.height + separation;

        const absX = area.x + x;
        const absY = area.y + y;

        block.moveBy({
            x: absX - blockArea.x,
            y: absY - blockArea.y,
        });
    }

    return { width, height };
}
