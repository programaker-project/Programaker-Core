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
    const trigger = builder.add_trigger('flow_utc_date', {id: 'trigger', args: []});

    // Streaming operation
    builder.add_getter('dynamic_text', { id: 'year', args: [[trigger, 0 ]] });
    builder.add_getter('dynamic_text', { id: 'month', args: [[trigger, 1 ]] });
    builder.add_getter('dynamic_text', { id: 'day', args: [[trigger, 2 ]] });
    builder.add_getter('dynamic_text', { id: 'dow', args: [[trigger, 3 ]] });

    const graph = builder.build();
    return graph;
}

describe('Flow-34: Show date on dynamic text.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const TIME_SVC = '0093325b-373f-4f1c-bace-4532cce79df4';
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor key: utc_date from_service: ${TIME_SVC})
                (services.ui.dynamic_text.year (flow-last-value trigger 0))
                `)),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor key: utc_date from_service: ${TIME_SVC})
                (services.ui.dynamic_text.month (flow-last-value trigger 1))
                `)),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor key: utc_date from_service: ${TIME_SVC})
                (services.ui.dynamic_text.day (flow-last-value trigger 2))
                `)),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor key: utc_date from_service: ${TIME_SVC})
                (services.ui.dynamic_text.dow (flow-last-value trigger 3))
                `))
        ]);
    });
});
