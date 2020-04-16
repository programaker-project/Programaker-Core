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
    return graph;
}

describe('Flow-06: Stepped loops.', () => {
    it('Validation should pass', async () => {
        expect(() => validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";
        const WEATHER_SVC = "536bf266-fabf-44a6-ba89-a0c71b8db608";

        are_equivalent_ast(compile(gen_flow()),
                           [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                (if (and (= (flow-last-value "source" 0)
                            11)
                         (= (flow-last-value "source" 1)
                            (flow-last-value "source" 2)
                            0))
                    ((jump-point "loop-start")
                     (call-service id: "${CHAT_SVC}"
                                   action: "send_message"
                                   values: ("-137414823"
                                            (call-service id: "${WEATHER_SVC}"
                                                          action: "get_today_max_in_place"
                                                          values: ("12/36/057/7"))))
                     (wait-seconds 1)
                     (jump-to "loop-start")
                     ))
                `
            )),
        ]);
    });
});
