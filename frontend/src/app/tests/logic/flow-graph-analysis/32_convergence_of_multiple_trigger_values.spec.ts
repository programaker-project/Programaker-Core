import { split_streaming_after_stepped } from '../../../flow-editor/graph_transformations';
import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

const SERVICE = 'placeholder';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Trigger
    const trigger1 = builder.add_trigger('simple_button', {id: 'trigger1'});
    const trigger2 = builder.add_trigger('simple_button', {id: 'trigger2'});
    (builder.nodes.trigger1.value as any).extra.textContent = '1';
    (builder.nodes.trigger2.value as any).extra.textContent = '2';

    // Simple operation
    const out_of_tree_op = builder.add_op('logging_add_log', { id: 'log', args: [] });

    trigger1.then(out_of_tree_op);
    trigger2.then(out_of_tree_op);

    builder.establish_connection(['trigger1', 1], ['log', 1]);
    builder.establish_connection(['trigger2', 1], ['log', 1]);

    const graph = builder.build();
    return graph;
}

describe('Flow-32: Convergence of multiple trigger values.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger1)
                (log "1")
                `)),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger2)
                (log "2")
                `
            ))

        ]);
    });
});
