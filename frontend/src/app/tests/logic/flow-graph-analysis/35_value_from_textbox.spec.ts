import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Trigger
    const trigger = builder.add_trigger('simple_button', {id: 'trigger', args: []});
    const textbox = builder.add_trigger('text_box', {id: 'textbox', args: []});

    // Simple operation
    trigger.then(b => b.add_op('logging_add_log', { id: 'log1', args: [[textbox, 1]] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-35 Value from textbox.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger)
                (log (data_ui_block_value textbox))
                `))
        ]);
    });
});
