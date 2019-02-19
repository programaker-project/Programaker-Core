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
        this.workspace.updateToolbox(this.toolboxXML);
    }
}