import { ResolvedCustomBlock } from "../custom_block";

export class ToolboxController {
    toolboxXML: HTMLElement;
    workspace: any;
    customBlocks: { [key: string]: ResolvedCustomBlock };

    constructor() {
        this.customBlocks = {};
    }

    addCustomBlocks(blocks: ResolvedCustomBlock[]) {
        for (const block of blocks) {
            this.customBlocks[block.id] = block;
        }
    }

    getBlockInfo(blockId: string) {
        return this.customBlocks[blockId];
    }

    setWorkspace(workspace: any) {
        this.workspace = workspace;
    }

    setToolbox(toolboxXML: HTMLElement) {
        this.toolboxXML = toolboxXML;
    }

    update() {
        if ((!this.workspace) || (!this.toolboxXML)) {
            return;
        }

        this.workspace.updateToolbox(this.toolboxXML);
    }

    isKnownBlock(blockType: string) {
        if (!blockType.startsWith('services.')) {
            // We probably only have doubts on service blocks
            //  as they can "dissapear".
            return true;
        }

        // It it's a service block, check if we have info
        //  about it.
        return this.getBlockInfo(blockType) !== undefined;
    }

    getStringVariables(): string[] {
        return this.workspace
            .getAllVariables()
            .filter((v, _i, _a) => {
                return v.type !== "list";
            })
            .map((v, _i, _a) => {
                return v.name;
            });
    }
}
