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
    const trigger = builder.add_trigger('trigger_on_message', {id: 'trigger', args: [], namespace: SERVICE});

    // Simple operation
    trigger.then(b => b.add_op('logging_add_log', { id: 'log', args: [[trigger, 1]] }));

    // Streaming operation
    builder.add_getter('simple_debug_output', { id: 'out', args: [[trigger, 1 ]] });

    const graph = builder.build();
    return graph;
}

describe('Flow-33: Parallel trigger to flow and stepped.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.${SERVICE}.trigger_on_message)
                (log (flow-last-value trigger 1))
                `)),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.${SERVICE}.trigger_on_message)
                (services.ui.simple_debug_output.out (flow-last-value trigger 1))
                `))

        ]);
    });
});
