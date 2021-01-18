import { FlowBlock } from "../flow_block";

export class CannotSetAsContentsError extends Error {
    public readonly problematicContents: FlowBlock[];

    constructor(message: string, problematicContents: FlowBlock[]) {
        super(message);

        this.name = "CannotSetAsContentsError";
        this.message = message;
        this.problematicContents = problematicContents
    }
}
