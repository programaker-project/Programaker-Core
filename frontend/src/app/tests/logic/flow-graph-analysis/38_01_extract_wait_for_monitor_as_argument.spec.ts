import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

const TIME_SVC = '0093325b-373f-4f1c-bace-4532cce79df4';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Source
    const source = builder.add_trigger('flow_utc_date', {id: 'time', args: []});

    // Stepper section operation
    const trigger = builder.add_trigger('simple_button', {id: 'button', args: []});
    trigger.then(b => b.add_op('logging_add_log', { id: 'logger', args: [[source, 1 ]] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-38: Extract wait-for-monitor as argument.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.button)
                (wait-for-monitor key: utc_date from_service: ${TIME_SVC})
                (log (flow-last-value time 1))
                `))
        ]);
    });
});
