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

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel1 = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');
    const channel2 = builder.add_enum_node(chat, 'get_known_channels', 'Personal', '-137414824');

    // First trigger
    // Stream section
    const source1 = builder.add_stream('flow_utc_time', {id: 'source1', message: 'UTC time'});
    const cond1 = builder.add_stream('flow_equals', {args: [[source1, 0], 11]});
    const trigger1 = builder.add_trigger('trigger_when_all_true', {args: [[cond1, 0]]});
    const body1 = builder.add_op('send_message', { namespace: chat,
                                                   args: [channel1,
                                                          [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                           args: [loc]
                                                                                                         }), 0]]
                                                 });

    // Second trigger
    const source2 = builder.add_stream('flow_utc_time', {id: 'source2', message: 'UTC time'});
    const cond2 = builder.add_stream('flow_equals', {args: [[source2, 1], [source2, 2], 0]});
    const trigger2 = builder.add_trigger('trigger_when_all_true', {args: [[cond2, 0]]});
    const body2 = builder.add_op('send_message', { namespace: chat,
                                                   args: [channel2,
                                                          [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                           args: [loc]
                                                                                                         }), 0]]
                                                 });
    const body3 = builder.add_op('send_message', { namespace: chat,
                                                   args: [channel2, "this in parallel"]
                                                 });

    // Common end
    const ending = builder.add_op('send_message', { namespace: chat,
                                                    args: [channel2,
                                                           "done"]
                                                  });

    trigger1.then(body1).then(body2);

    builder.add_fork(trigger2, [body2, body3]);
    body2.then(ending);

    const joiner = builder.add_trigger('trigger_when_all_completed', {args: [[ ending, 'pulse' ], [body3, 'pulse']]});
    joiner.then(f => f.add_op('send_message', { namespace: chat,
                                                args: [channel2, 'completed']
                                              }));

    const graph = builder.build();
    return graph;
}

describe('Flow-13: Fork to different branch then join.', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError:.*Joins can only be done between flows that have previously forked.*/g)
    });
});
