import { FlowGraph } from '../../../flow-editor/flow_graph';
import { validate } from '../../../flow-editor/graph_validation';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source1 = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const source2 = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});

    const trigger1 = builder.add_trigger('trigger_on_signal', {args: [[source1, 0]]});
    const trigger2 = builder.add_trigger('trigger_on_signal', {args: [[source2, 0]]});

    // Stepped section
    const branch1 = builder.add_op('control_wait', { args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('control_wait', { args: [ 2 ]
                                                      });

    builder.add_fork(trigger1, [branch1, branch2]);
    trigger2.then(branch2);

    // Join branch 1 and 2
    const joiner = builder.add_trigger('trigger_when_all_completed', {args: [[ branch1, 'pulse' ], [branch2, 'pulse']]});
    joiner.then(f => f.add_op('control_wait', { args: [ 9 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-23: Calling from second source to forked flow fails.', () => {
    it('Validation should pass', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError: Block .* can get to Join .* with no fork.*/i)
    });
});
