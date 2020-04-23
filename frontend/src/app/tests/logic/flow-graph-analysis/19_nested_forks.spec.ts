import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';
import { lift_common_ops } from '../../../flow-editor/graph_transformations';

export function process_flow(graph: FlowGraph): FlowGraph {
    // Used by visualizing script to produce a processed version specific for this test
    return lift_common_ops(graph);
}

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const cond = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});
    const branch1 = builder.add_op('op_wait_seconds', { id: 'branch1',
                                                        args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('op_wait_seconds', { id: 'branch2',
                                                        args: [ 2 ]
                                                      });

    builder.add_fork(trigger, [branch1, branch2 ]);

    const branch3 = builder.add_op('op_wait_seconds', { args: [ 3 ] });
    const branch4 = builder.add_op('op_wait_seconds', { args: [ 4 ] });

    builder.add_fork(branch2, [branch3, branch4 ]);

    const branch5 = builder.add_op('op_wait_seconds', { args: [ 5 ] });
    const branch6 = builder.add_op('op_wait_seconds', { args: [ 6 ] });

    builder.add_fork(branch4, [branch5, branch6 ]);


    // Join branch 1 and 6
    const joiner16 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch1, 'pulse' ], [branch6, 'pulse']]})
        .then(f => f.add_op('op_wait_seconds', { args: [ 16 ] }));

    // Join branch 3 and 5
    const joiner35 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch3, 'pulse' ], [branch5, 'pulse']]})
        .then(f => f.add_op('op_wait_seconds', { args: [ 35 ] }));

    const joiner_all = builder.add_trigger('trigger_when_all_completed', {args: [[ joiner16, 'pulse' ],
                                                                                 [ joiner35, 'pulse' ]
                                                                                ]});
    joiner_all.then(f => f.add_op('op_wait_seconds', { args: [ 9 ]}))

    const graph = builder.build();
    return graph;
}

describe('Flow-19: Nested forks.', () => {
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
                ;; Operations 2 and 4 are lifted out of common joins
                ((wait-seconds 2)
                 (wait-seconds 4)
                 (fork
                  ;; Branch 1 & 6
                  ((fork
                    ((wait-seconds 1))
                    ((wait-seconds 6)))
                   (wait-seconds 16))
                  ;; Branch 2
                  ((fork
                    ((wait-seconds 5))
                    ((wait-seconds 3)))
                   (wait-seconds 35)))
                 (wait-seconds 9)
                 ))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
