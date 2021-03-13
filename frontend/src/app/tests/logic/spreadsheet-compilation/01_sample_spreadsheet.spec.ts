import { compile_spreadsheet } from "../../../program-editors/spreadsheet-editor/spreadsheet-compiler";
import { ISpreadsheetToolbox } from "../../../program-editors/spreadsheet-editor/spreadsheet-toolbox";
import { are_equivalent_ast } from "../flow-graph-analysis/utils.spec";
import { gen_compiled } from "../scaffolding/graph-analysis-tools";
import { dsl_to_ast } from "../scaffolding/graph-analysis-tools-ast-dsl";
import { FakeSpreadsheetToolbox } from './fake-toolbox';

const BRIDGE_ID = '7e90d22c-4518-4e93-9106-e69e4a85bc34';
const BRIDGE_NAME = "matrix";

export function gen_sheet(): {[key: string]: string} {
    return {
        "C2":"=7e34_on_new_message()",
        "E2": "=7e34_answer_message(\"Hello\" + C2 + \" \" + 123, 7e34_get_value(123 + 456))",
    }
}

export function gen_toolbox() : ISpreadsheetToolbox {
    return new FakeSpreadsheetToolbox([
        {
            id: BRIDGE_ID,
            name: BRIDGE_NAME,
            blocks: [
                {
                    id: '7e34_on_new_message',
                    message: 'On new message, save %1',
                },
                {
                    id: '7e34_answer_message',
                    message: 'Respond %1',
                },
                {
                    id: '7e34_get_value',
                    message: 'Get value',
                },
            ]
        }
    ]);
}

describe('Spreadsheet-01: Sample spreadsheet.', () => {
    it('Should be able to compile', async () => {
        are_equivalent_ast(compile_spreadsheet(gen_sheet(), gen_toolbox()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.${BRIDGE_ID}.on_new_message)
                (services.${BRIDGE_ID}.answer_message (+ "Hello" (flow-last-value "C2" 0) " " 123)
                                                      (services.${BRIDGE_ID}.get_value (+ 123 456)) )
                `))
        ]);
    });
});
