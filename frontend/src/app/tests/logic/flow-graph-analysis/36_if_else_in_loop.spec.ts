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

    // Condition & Check
    const cond = builder.add_stream('operator_equals', { id: 'equals', args: [1, 2, 11]});
    const check = builder.add_op('control_if_else', { id: 'check', args: [ [cond, 0] ] });

    //   /----------------\
    //   ↓                |
    // Trigger → wait 1 -/
    //      |
    //      \--→ wait 2 (finish)
    //
    trigger.then(check).then(f => f.add_op('control_wait', { id: 'true-branch', args: [ 1 ]})).then(check);
    check.then(f => f.add_op('control_wait', { id: 'false-branch', args: [ 2 ] }), 1);

    const graph = builder.build();
    return graph;
}

describe('Flow-36: If-else in loop.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger)
                (jump-point "check")
                (if (= 1 2 11)
                    ((wait-seconds 1)
                     (jump-to "check")
                     )
                  ((wait-seconds 2)))
                `))
        ]);
    });
});
