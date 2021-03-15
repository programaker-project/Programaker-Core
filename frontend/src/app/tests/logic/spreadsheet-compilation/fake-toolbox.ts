import { ISpreadsheetToolbox, CategoryDef, simplify_id } from "../../../program-editors/spreadsheet-editor/spreadsheet-toolbox";
import { ResolvedCustomBlock } from "../../../custom_block";

export class FakeSpreadsheetToolbox implements ISpreadsheetToolbox {
    public categories: CategoryDef[];
    public nonEmptyCategories: CategoryDef[];
    public blockMap: { [key: string]: { cat: CategoryDef; block: ResolvedCustomBlock; }; };

    constructor (categories: CategoryDef[], blocks: ResolvedCustomBlock[]) {
        this.categories = categories;
        this.nonEmptyCategories = categories.filter(c => c.blocks.length > 0);
        this.blockMap = {};

        for (const block of blocks) {
            this.blockMap[simplify_id(block)] = {
                cat: categories.find(c => c.id === block.service_port_id),
                block: block
            };
        }
    }
}
