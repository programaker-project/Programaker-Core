import { ISpreadsheetToolbox, CategoryDef } from "../../../program-editors/spreadsheet-editor/spreadsheet-toolbox";
import { ResolvedCustomBlock } from "app/custom_block";

export class FakeSpreadsheetToolbox implements ISpreadsheetToolbox {
    public categories: CategoryDef[];
    public nonEmptyCategories: CategoryDef[];
    public blockMap: { [key: string]: { cat: CategoryDef; block: ResolvedCustomBlock; }; };

    constructor (categories: CategoryDef[]) {
        this.categories = categories;
        this.nonEmptyCategories = categories.filter(c => c.blocks.length > 0);
        this.blockMap = {};

        for (const cat of categories) {
            for (const block of cat.blocks) {
                this.blockMap[block.id] = { cat, block };
            }
        }
    }
}
