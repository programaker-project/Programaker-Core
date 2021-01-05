import { Area2D, FlowBlock, Position2D } from "../../flow_block";
import { UiFlowBlock, UiFlowBlockHandler } from "../ui_flow_block";
import { cleanestTree, getElementsInGroup, getRect, getShallowElementsInGroup } from "./responsive_page";
import { CutNode, CutTree } from "./ui_tree_repr";
import { manipulableAreaToArea2D } from "./utils";
import { ContainerFlowBlock } from "../container_flow_block";

export const SEPARATION = 25;

export function PositionResponsiveContents(handler: UiFlowBlockHandler, blocks: FlowBlock[], allBlocks: FlowBlock[], offset: Position2D): CutTree {
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


    PositionTreeContentsFromTree(tree, blockMap, offset);

    return tree;
}

function PositionTreeContentsFromTree(tree: CutTree, blocks: {[key: string]: UiFlowBlock}, offset: Position2D, nonTopLevel?: boolean) {
    if (!(tree as CutNode).cut_type) {
        return;
    }

    const cTree = tree as CutNode;
    const toCenter: UiFlowBlock[] = [];
    console.error('>>', cTree.cut_type, "|", offset.x, offset.y);

    // First position subtrees
    const subTreeOffset: Position2D = {
        x: offset.x + (nonTopLevel ? 0 : SEPARATION),
        y: offset.y + (nonTopLevel ? 0 : SEPARATION),
    };

    for (const group of cTree.groups) {
        let area: Area2D;

        if ((group as CutNode).block_id) {
            // Treat as a single block
            const block = blocks[(group as CutNode).block_id];
            area = block.getBodyArea();

            console.log("Y - ST", subTreeOffset.y, "A", area.y);

            const mov = {
                x: subTreeOffset.x - area.x,
                y: subTreeOffset.y - area.y,
            };

            if (block.isAutoresizable()) {
                // Don't push away from borders
                if (cTree.cut_type === 'vbox') {
                    mov.x = 0;
                }
                else if (cTree.cut_type === 'hbox') {
                    mov.y = 0;
                }
            }

            console.log("MoveBox", mov, block.options.id);
            block.moveBy(mov);
            console.log("=>", JSON.stringify((block as any).position));
            toCenter.push(block);
        }
        else {
            // Treat as a group
            // console.log("C", contents.map(id => blocks[id]));

            const subOffset = { x: subTreeOffset.x, y: subTreeOffset.y };

            // if (cTree.cut_type === 'hbox') {
            //     subOffset.y -= SEPARATION;
            // }
            // else {
            //     subOffset.x -= SEPARATION;
            // }

            // console.log(">>>>")
            PositionTreeContentsFromTree(group, blocks, subOffset, true);
            // console.log("<<<<");

            const contents = getElementsInGroup(group);
            area = manipulableAreaToArea2D(getRect(contents.map(id => blocks[id])));
            const mov = {
                x: subTreeOffset.x - area.x,
                y: subTreeOffset.y - area.y,
            };

            const elementsToMove = getShallowElementsInGroup(group).map(id => blocks[id]);
            console.log("MOV", elementsToMove, mov);

            for (const block of elementsToMove) {
                console.error("MoveBy", mov, block.options.id);

                if (block.isAutoresizable()) {

                    // This really means "is this considered on PositionTreeContentsFromTree()"
                    // TODO: Update this approximation when testing with simple_cards
                    if (block instanceof ContainerFlowBlock) {
                        continue;
                    }

                    // Don't push away from borders
                    if (cTree.cut_type === 'vbox') {
                        mov.x = 0;
                    }
                    else if (cTree.cut_type === 'hbox') {
                        mov.y = 0;
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
            console.error("V>", subTreeOffset.y, area.height, SEPARATION);
            subTreeOffset.y += area.height + SEPARATION;
            console.error("RV>", subTreeOffset.y);
        }
        else if (cTree.cut_type === 'hbox') {
            console.error("H>", subTreeOffset.x, area.width, SEPARATION);
            subTreeOffset.x += area.width + SEPARATION;
            console.error("RH>", subTreeOffset.x);
        }
    }

    const fullArea = manipulableAreaToArea2D(getRect(getElementsInGroup(cTree).map(id => blocks[id])));
    for (const elem of toCenter) {
        const eArea = elem.getBodyArea();

        const mov = { x: 0, y: 0 };
        if (cTree.cut_type === 'vbox') {
            const xPos = fullArea.x + ((fullArea.width - eArea.width) / 2)
            mov.x = xPos - eArea.x;
        }
        else if (cTree.cut_type === 'hbox') {
            const yPos = fullArea.y + ((fullArea.height - eArea.height) / 2)
            mov.y = yPos - eArea.y;
        }

        elem.moveBy(mov);
    }

    console.error('<< ', cTree.cut_type);
}

export function PositionHorizontalContents (handler: UiFlowBlockHandler, blocks: FlowBlock[], area: Area2D): { width: number, height: number } {
    const blockAreas: [UiFlowBlock, Area2D][] = (
        blocks
            .filter(b => b instanceof UiFlowBlock)
            .map((b: UiFlowBlock) => [b, b.getBodyArea()]));

    const minBlockHeights = blockAreas.filter(([b, _]) => !b.isAutoresizable())
        .map(([_, a]) => a.height);
    const maxHeight = Math.max(...minBlockHeights);

    const blockWidths = blockAreas.map(([_, a]) => a.width);
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
    console.log("Vert");

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
