import { FlowWorkspace } from "app/flow-editor/flow_workspace";
import { Area2D } from "app/flow-editor/flow_block";

export function doesNotChangePositionsOnReposition(workspace: FlowWorkspace, blocks: string[]) {
    // Get all block's positions
    const prevPos: [string, Area2D][] = blocks.map( id => [id, workspace.getBlock(id).getBodyArea() ] );

    workspace.repositionAll();

    const diffs = prevPos.map(([id, prev]) => {
        const after = workspace.getBlock(id).getBodyArea();

        return {
            x: Math.abs(after.x - prev.x),
            y: Math.abs(after.y - prev.y),
            width: Math.abs(after.width - prev.width),
            height: Math.abs(after.height - prev.height),
        }
    })

    const mov = {
        x: Math.max(...diffs.map(d => d.x)),
        y: Math.max(...diffs.map(d => d.y)),
        width: Math.max(...diffs.map(d => d.width)),
        height: Math.max(...diffs.map(d => d.height)),
    };


    if ((Math.abs(mov.x) >= 1) && (Math.abs(mov.y) >= 1) && (Math.abs(mov.width) >= 1) && (Math.abs(mov.height) >= 1)) {
        fail("Movement on iteration that should not change positions: " + JSON.stringify(mov));
    }
}
