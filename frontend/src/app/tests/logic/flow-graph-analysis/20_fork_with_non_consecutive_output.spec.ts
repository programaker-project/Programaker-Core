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
    const cond = builder.add_stream('operator_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});

    const branch2 = builder.add_op('control_wait', { id: 'branch2',
                                                        args: [ 2 ]
                                                      });

    const branch3 = builder.add_op('control_wait', { id: 'branch3',
                                                        args: [ 3 ]
                                                      });

    builder.add_fork(trigger, [null, branch2, branch3 ]);

    // Join branch 2 and 3
    const joiner23 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch2, 'pulse' ], [branch3, 'pulse']]});
    joiner23.then(f => f.add_op('control_wait', { id: 'joiner23', args: [ 23 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-20: Fork with non consecutive output.', () => {
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
                ((fork
                  ((wait-seconds 2))

                  ((wait-seconds 3)))
                 (wait-seconds 23)))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
