import { FlowGraph } from '../../../flow-editor/flow_graph';
import { validate } from '../../../flow-editor/graph_validation';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { compile } from '../../../flow-editor/graph_analysis';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { are_equivalent_ast } from './utils.spec';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const cond = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});

    const if_true = builder.add_op('op_wait_seconds', { args: [ 1 ] });
    const if_false = builder.add_op('op_wait_seconds', { args: [ 2 ] });
    trigger.then_id(builder.add_if(if_true, if_false, { cond: [cond, 0] }))

    const branch3 = builder.add_op('op_wait_seconds', { args: [ 3 ]
                                                      });

    const branch4 = builder.add_op('op_wait_seconds', { id: 'branch2',
                                                        args: [ 4 ]
                                                      });


    builder.add_fork(if_true, [branch3, branch4]);

    // Join branch 1 and 2
    const fork_joiner = builder.add_trigger('trigger_when_all_completed', {args: [[branch3, 'pulse'], [ branch4, 'pulse']]});
    const if_joiner = builder.add_trigger('trigger_when_all_completed', {args: [[ fork_joiner, 'pulse' ], [if_false, 'pulse']]});
    if_joiner.then(f => f.add_op('op_wait_seconds', { args: [ 9 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-21-10: IF then Fork, close merging ALL (wrong form).', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError:.*can get to Join.*with no fork.*$/i)
    });
});
