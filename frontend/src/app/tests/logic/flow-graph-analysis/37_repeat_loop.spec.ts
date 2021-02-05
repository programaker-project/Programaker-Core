import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(options?: { two_ops: boolean }): FlowGraph {
    if (!options) { options = { two_ops: false }; }

    const builder = new GraphBuilder();

    // Trigger
    const trigger = builder.add_trigger('simple_button', {id: 'trigger', args: []});

    // Loop
    const loop = builder.add_op('control_repeat', { id: 'loop', args: [ 3 ] });

    //
    // Loop → wait 1 (iteration completed)
    //      |
    //      \→ wait 2 (finish)
    //

    const log = builder.add_op('logging_add_log', { id: 'in-loop', args: [ [loop, 1] ]});

    trigger
        .then(loop)
        .then(log)

    if (options.two_ops) {
        log.then(f => f.add_op('control_wait', { args: [ 1 ] }));
    }

    loop.then(f => f.add_op('logging_add_log', { id: 'out-of-loop', args: [ [loop, 1] ] }), 2);

    const graph = builder.build();
    return graph;
}

describe('Flow-37: Repeat loop.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile with one op', async () => {
        are_equivalent_ast(compile(gen_flow({ two_ops: false })), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger)
                (repeat 3
                    (log (flow-last-value "loop" 1)))
                (log (flow-last-value "loop" 1))
                `))
        ]);
    });

    it('Should be able to compile with two ops', async () => {
        are_equivalent_ast(compile(gen_flow({ two_ops: true })), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.ui.simple_button.trigger)
                (repeat 3
                    (log (flow-last-value "loop" 1))
                    (wait-seconds 1))
                (log (flow-last-value "loop" 1))
                `))
        ]);
    });
});
