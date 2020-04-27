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
    const cond = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});
    const branch1 = builder.add_op('op_wait_seconds', { id: 'branch1',
                                                        args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('op_wait_seconds', { id: 'branch2',
                                                        args: [ 2 ]
                                                      });

    trigger.then_id(builder.add_if(branch1, branch2, {
        cond: [ cond, 0 ]
    }))

    // Join branch 1 and 2
    const joiner12 = builder.add_trigger('trigger_when_all_completed', {args: [[branch1, 'pulse'], [ branch2, 'pulse' ]]});
    joiner12.then(f => f.add_op('op_wait_seconds', { id: 'joiner12', args: [ 12 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-21-2: If with fork closer (when any completed).', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError:.*can get to Join.*with no fork.*$/)
    });
});
