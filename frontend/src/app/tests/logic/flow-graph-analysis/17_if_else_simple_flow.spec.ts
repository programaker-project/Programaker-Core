import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const stream_cond = builder.add_stream('operator_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[stream_cond, 0]]});
    const root = trigger.then(f => f.add_op('control_wait', { args: [ 0 ] }))
    const branch1_top = builder.add_op('control_wait', { args: [ 1 ] });
    const branch1_bot = branch1_top.then(f => f.add_op('control_wait', { args: [ 1.1 ]}));

    const branch2_top = builder.add_op('control_wait', { args: [ 2 ] });
    const branch2_bot = branch2_top.then(f => f.add_op('control_wait', { args: [ 2.1 ]}));

    const if_cond = builder.add_if(branch1_top, branch2_top,
                                   { cond: f => f.add_stream('operator_equals', {args: [1, 1]})
                                   });

    root.then_id(if_cond);

    // Join branches
    const joiner = builder.add_trigger('trigger_when_first_completed', {args: [
        [ branch1_bot, 'pulse' ],
        [ branch2_bot, 'pulse' ],
    ]});
    joiner.then(f => f.add_op('control_wait', { id: 'joiner', args: [ 3 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-17: Simple If-Else flow.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });


    it('Should be able to compile', async () => {
        const compiled_flow = compile(gen_flow());

        const dsl_ast = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (if (and (= (flow-last-value "source" 0)
                        11)
                     )
                ((wait-seconds 0)
                  (if (= 1 1)
                      ((wait-seconds 1)
                       (wait-seconds 1.1))
                    ((wait-seconds 2)
                     (wait-seconds 2.1)))
                  (control_wait 3)))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
