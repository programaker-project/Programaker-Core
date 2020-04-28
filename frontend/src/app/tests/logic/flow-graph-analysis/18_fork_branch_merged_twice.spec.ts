import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { lift_common_ops } from '../../../flow-editor/graph_transformations';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

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
    const branch3 = builder.add_op('op_wait_seconds', { id: 'branch3',
                                                        args: [ 3 ] });

    builder.add_fork(trigger, [branch1, branch2, branch3 ]);

    const branch4 = builder.add_op('op_wait_seconds', { id: 'branch4',
                                                        args: [ 2.4 ]
                                                      })

    const branch5 = builder.add_op('op_wait_seconds', { id: 'branch5',
                                                        args: [ 2.5 ]
                                                      })

    builder.add_fork(branch2, [branch4, branch5]);

    // Join branch 1 and 2
    const joiner12 = builder.add_trigger('trigger_when_all_completed', { id: 'joiner12',
                                                                         args: [[ branch1, 'pulse' ], [branch4, 'pulse']]})
        .then(f => f.add_op('op_wait_seconds', { id: 'branch12', args: [ 12 ] }));

    // Join branch 2 and 3
    const joiner23 = builder.add_trigger('trigger_when_all_completed', { id: 'joiner23',
                                                                         args: [[ branch5, 'pulse' ], [branch3, 'pulse']]})
        .then(f => f.add_op('op_wait_seconds', { id: 'branch23', args: [ 23 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-18: Fork branch merged twice.', () => {
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
                ;; Operation 2 is lifted out of common joins
                ((wait-seconds 2)
                 (fork
                  ((fork
                    ((wait-seconds 1))
                    ((wait-seconds 2.4)))
                   (wait-seconds 12))
                  ((fork
                    (wait-seconds 3)
                    (wait-seconds 2.5))
                   (wait-seconds 23))
                  )))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
