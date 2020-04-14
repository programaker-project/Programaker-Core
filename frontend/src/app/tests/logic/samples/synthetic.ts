import { FlowGraph } from '../../../flow-editor/flow_graph';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { convert_to_graphviz } from '../scaffolding/utils';

export function synth_01_simple_flow(): FlowGraph {

    const builder = new GraphBuilder();

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');

    // Stream section
    const source = builder.add_stream('flow_utc_time', {message: 'UTC time'});
    const cond1 = builder.add_stream('flow_equals', {args: [[source, 1], [source, 2], 0]});
    const cond2 = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    builder.add_trigger('trigger_when_all_true', {args: [[cond1, 0], [cond2, 0]]})
        .then(f => f.add_op('send_message', { namespace: chat,
                                              args: [channel,
                                                     [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                      args: [loc]
                                                                                                    }), 0]]
                                            }));

    const graph = builder.build();
    return graph;
}

export function synth_02_lone_block(): FlowGraph {
    const builder = new GraphBuilder();

    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    builder.add_op('send_message', { id: 'lone', namespace: chat });

    const graph = builder.build();
    return graph;
}

export function synth_03_no_start_pulse(): FlowGraph {
    const builder = new GraphBuilder();

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');

    // Stepped section
    builder.add_op('send_message', { namespace: chat,
                                     id: 'not-started',
                                     args: [channel,
                                            [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                             args: [loc]
                                                                                           }), 0]]
                                   });

    const graph = builder.build();
    return graph;
}

export function synth_04_no_start_loop(): FlowGraph {
    const builder = new GraphBuilder();

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');

    // Stepped section
    const msg1 = builder.add_op('send_message', { namespace: chat,
                                                  id: 'first',
                                                  args: [channel,
                                                         [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                          args: [loc]
                                                                                                        }), 0]]
                                                });

    const msg2 = builder.add_op('send_message', { namespace: weather,
                                                  id: 'second',
                                                  args: [channel,
                                                         [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                          args: [loc]
                                                                                                        }), 0]]
                                                });

    msg1.then(msg2).then(msg1);

    const graph = builder.build();
    return graph;
}

export function synth_05_multiple_streams_in(): FlowGraph {

    const builder = new GraphBuilder();

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');

    // Stream section
    const source1 = builder.add_stream('flow_utc_time', {id: 'source1', message: 'UTC time'});
    const cond1 = builder.add_stream('flow_equals', {args: [[source1, 0], 11]});

    const source2 = builder.add_stream('flow_utc_time', {id: 'source2', message: 'UTC time'});
    const cond2 = builder.add_stream('flow_equals', {args: [[source2, 1], [source2, 2], 0]});

    // Stepped section
    builder.add_trigger('trigger_when_all_true', {id: 'trigger', args: [[cond1, 0], [cond2, 0]]})
        .then(f => f.add_op('send_message', { namespace: chat,
                                              args: [channel,
                                                     [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                      args: [loc]
                                                                                                    }), 0]]
                                            }));

    const graph = builder.build();
    // console.log(convert_to_graphviz(graph));

    return graph;
}

export function synth_06_stepped_loops(): FlowGraph {
    const builder = new GraphBuilder();

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const cond1 = builder.add_stream('flow_equals', {args: [[source, 0], 11]});
    const cond2 = builder.add_stream('flow_equals', {args: [[source, 1], [source, 2], 0]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {id: 'trigger', args: [[cond1, 0], [cond2, 0]]})
    const send_message = builder.add_op('send_message', { namespace: chat,
                                                          id: 'loop-start',
                                                          args: [channel,
                                                                 [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                                  args: [loc]
                                                                                                                }), 0]]
                                                        });
    const wait_1sec = builder.add_op('op_wait_seconds', { args: [1]
                                                        });

    trigger.then(send_message).then(wait_1sec).then(send_message).then(wait_1sec);

    const graph = builder.build();
    // console.log('\n' + convert_to_graphviz(graph) + '\n');

    return graph;
}
