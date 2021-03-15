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
                    message: 'When I say something in any channel. Set %1',
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
    ], [
		{
			"subkey": null,
			"save_to": {
				"type": "argument",
				"index": 0
			},
			"message": "When I say something in any channel. Set %1",
			"key": "on_new_message",
			"function_name": "on_new_message",
			"service_port_id": BRIDGE_ID,
            "id": 'services.' + BRIDGE_ID + '.' + "on_new_message",
			"block_type": "trigger",
            block_result_type: null,
			"block_id": "on_new_message",
			"arguments": [
				{
					"var_type": "string",
					"type": "variable",
					"default_value": "undefined",
					"class": "single"
				}
			]
		},
		{
			"save_to": null,
			"message": "Respond %1",
			"function_name": "answer_message",
			"block_type": "operation",
			"block_result_type": null,
            "id": 'services.' + BRIDGE_ID + '.' + "answer_message",
			"service_port_id": BRIDGE_ID,
			"block_id": "answer_message",
			"arguments": [
				{
					"type": "string",
					"default_value": "Hello",
				}
			]
		},
		{
			"save_to": null,
			"message": "Get value",
			"function_name": "get_value",
			"block_type": "getter",
            "id": 'services.' + BRIDGE_ID + '.' + "get_value",
			"service_port_id": BRIDGE_ID,
			"block_result_type": "string",
			"block_id": "get_value",
			"arguments": [],
		}
    ]);
}

describe('Spreadsheet-01: Sample spreadsheet.', () => {
    it('Should be able to compile', async () => {
        are_equivalent_ast(compile_spreadsheet(gen_sheet(), gen_toolbox()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.${BRIDGE_ID}.on_new_message)
                (call-service id: ${BRIDGE_ID}
                              action: answer_message
                              values: ( (+ (+ (+ "Hello" (flow-last-value "C2" 1)) " ") 123)
                                        (call-service id: ${BRIDGE_ID} action: get_value values: ((+ 123 456)))))
                `))
        ]);
    });
});
