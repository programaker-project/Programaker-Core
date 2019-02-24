export class ToolboxController {
    toolboxXML: HTMLElement;
    workspace: any;

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