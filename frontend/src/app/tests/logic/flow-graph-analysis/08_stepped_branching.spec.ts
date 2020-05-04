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

    // Stream section
    const source = builder.add_stream('flow_utc_time', {'id': 'source', message: 'UTC time'});
    const cond1 = builder.add_stream('operator_equals', {args: [[source, 1], [source, 2], 0]});
    const cond2 = builder.add_stream('operator_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond1, 0], [cond2, 0]]});
    const branch1 = builder.add_op('send_message', { namespace: chat,
                                                     args: [channel1,
                                                            [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                             args: [loc]
                                                                                                           }), 0]]
                                                   });

    const branch2 = builder.add_op('send_message', { namespace: chat,
                                                     args: [channel2,
                                                        [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                         args: [loc]
                                                                                                       }), 0]]
                                                   });

    builder.add_fork(trigger, [branch1, branch2]);

    const graph = builder.build();
    return graph;
}

describe('Flow-08: Stepped branching.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";
        const WEATHER_SVC = "536bf266-fabf-44a6-ba89-a0c71b8db608";

        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                (if (and (= (flow-last-value "source" 0)
                            11)
                         (= (flow-last-value "source" 1)
                            (flow-last-value "source" 2)
                            0))
                    ((fork
                      (call-service id: "${CHAT_SVC}"
                                    action: "send_message"
                                    values: ("-137414823"
                                             (call-service id: "${WEATHER_SVC}"
                                                           action: "get_today_max_in_place"
                                                           values: ("12/36/057/7"))))
                      (call-service id: "${CHAT_SVC}"
                                    action: "send_message"
                                    values: ("-137414824"
                                             (call-service id: "${WEATHER_SVC}"
                                                           action: "get_today_max_in_place"
                                                           values: ("12/36/057/7"))))
                      )))
                `
            )),
        ]);
    });
});
