import { Injectable } from "@angular/core";
import { ResolvedCustomBlock } from "app/custom_block";

@Injectable()
export class FakeCustomBlockService {
    constructor() {}

    public async getCustomBlocksOnProgram(programId: string, skip_resolve_argument_options?: boolean): Promise<ResolvedCustomBlock[]> {
        return Promise.resolve([]);
    }
}
