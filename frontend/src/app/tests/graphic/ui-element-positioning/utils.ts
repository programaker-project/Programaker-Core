import { FlowWorkspace } from "app/flow-editor/flow_workspace";
import { Area2D } from "app/flow-editor/flow_block";
import { maxKey } from "app/flow-editor/utils";

export function doesNotChangePositionsOnReposition(workspace: FlowWorkspace, blocks: string[]) {
    // Get all block's positions
    const prevPos: [string, Area2D][] = blocks.map( id => [id, workspace.getBlock(id).getBodyArea() ] );

    workspace.repositionAll();

    const diffs = prevPos.map(([id, prev]) => {
        const after = workspace.getBlock(id).getBodyArea();

        return {
            id: id,
            x: Math.abs(after.x - prev.x),
            y: Math.abs(after.y - prev.y),
            width: Math.abs(after.width - prev.width),
            height: Math.abs(after.height - prev.height),
        }
    })

    const x = maxKey(diffs, (e => e.x));
    const y = maxKey(diffs, (e => e.x));
    const width = maxKey(diffs, (e => e.width));
    const height = maxKey(diffs, (e => e.height));

    const mov = {
        x: { id: x.id, v: x.x },
        y: { id: y.id, v: y.y },
        width: { id: width.id, v: width.width },
        height: { id: height.id, v: height.height },
    };

    if ((mov.x.v >= 1) || (mov.y.v >= 1) || (mov.width.v >= 1) || (mov.height.v >= 1)) {
        // @ts-ignore Avoid importing this from assert() as that one has 2 more parameters
        fail("Movement on iteration that should not change positions: " + JSON.stringify(mov));
    }
}
