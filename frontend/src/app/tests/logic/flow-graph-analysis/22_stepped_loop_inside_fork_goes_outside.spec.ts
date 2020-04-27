import { FlowGraph } from '../../../flow-editor/flow_graph';
import { validate } from '../../../flow-editor/graph_validation';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const cond = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});
    const before_fork = builder.add_op('op_wait_seconds', { args: [ 0 ]
                                                          });

    const branch1 = builder.add_op('op_wait_seconds', { args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('op_wait_seconds', { args: [ 2 ]
                                                      });

    const branch3 = builder.add_op('op_wait_seconds', { args: [ 3 ]
                                                      });

    trigger.then(before_fork);
    builder.add_fork(before_fork, [branch1, branch2, branch3 ]);

    branch3.then(before_fork);

    // Join branch 1 and 2
    const joiner12 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch1, 'pulse' ], [branch2, 'pulse']]});
    joiner12.then(f => f.add_op('op_wait_seconds', { id: 'joiner12', args: [ 11 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-22: Stepped loop inside fork goes outside of it.', () => {
    it('Validation should pass', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError:.*Loop around Fork blocks not allowed.*/i)
    });
});
