import { split_streaming_after_stepped } from '../../../flow-editor/graph_transformations';
import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function process_flow(graph: FlowGraph): FlowGraph {
    // Used by visualizing script to produce a processed version specific for this test
    return split_streaming_after_stepped(graph);
}

const SERVICE = 'placeholder';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Trigger
    const trigger = builder.add_trigger('simple_button', {id: 'trigger', args: []});

    // Simple operation
    const operation = builder.add_op('op_with_output', { id: 'op', args: ["Triggered"], namespace: SERVICE });
    const out_of_tree_op = builder.add_op('logging_add_log', { id: 'log', args: ["Out of tree"] });

    trigger
        .then(operation)
        .then(out_of_tree_op)
        ;

    builder.add_getter('dynamic_text', { id: 'out', args: [[operation, 1]] });

    const graph = builder.build();
    return graph;
}

describe('Flow-31: Update streaming block from stepped flow.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger)
                (call-service id: ${SERVICE} action: op_with_output values: ("Triggered"))
                (log "Out of tree")
                `)),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (on-block-run "op" 1)
                (services.ui.dynamic_text.out (flow-last-value "op" 1))
                `
            ))

        ]);
    });
});
