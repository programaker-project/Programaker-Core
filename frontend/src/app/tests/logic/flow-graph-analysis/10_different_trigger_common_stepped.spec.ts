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

    // First trigger
    // Stream section
    const source1 = builder.add_stream('flow_utc_time', {id: 'source1', message: 'UTC time'});
    const cond1 = builder.add_stream('operator_equals', {args: [[source1, 0], 11]});
    const trigger1 = builder.add_trigger('trigger_when_all_true', {args: [[cond1, 0]]});


    // Second trigger
    const source2 = builder.add_stream('flow_utc_time', {id: 'source2', message: 'UTC time'});
    const cond2 = builder.add_stream('operator_equals', {args: [[source2, 1], [source2, 2], 0]});
    const trigger2 = builder.add_trigger('trigger_when_all_true', {args: [[cond2, 0]]});

    // Stepped section
    const body = builder.add_op('send_message', { namespace: chat,
                                                  args: [channel1,
                                                         [(b) => b.add_getter('get_today_max_in_place', { namespace: weather,
                                                                                                          args: [loc]
                                                                                                        }), 0]]
                                                });

    trigger1.then(body);
    trigger2.then(body);

    const graph = builder.build();
    return graph;
}

describe('Flow-10: Different trigger common stepped.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";
        const WEATHER_SVC = "536bf266-fabf-44a6-ba89-a0c71b8db608";

        const join = compile(gen_flow());

        const dsl_ast_branch1 = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (if (and (= (flow-last-value "source1" 0)
                        11)
                     )
                ((call-service id: "${CHAT_SVC}"
                               action: "send_message"
                               values: ("-137414823"
                                        (call-service id: "${WEATHER_SVC}"
                                                      action: "get_today_max_in_place"
                                                      values: ("12/36/057/7"))))))
            `
        );

        const dsl_ast_branch2 = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (if (and (= (flow-last-value "source2" 1)
                        (flow-last-value "source2" 2)
                        0))
                ((call-service id: "${CHAT_SVC}"
                               action: "send_message"
                               values: ("-137414823"
                                        (call-service id: "${WEATHER_SVC}"
                                                      action: "get_today_max_in_place"
                                                      values: ("12/36/057/7"))))))
            `
        );

        const from_ast = [gen_compiled(dsl_ast_branch1), gen_compiled(dsl_ast_branch2)];

        are_equivalent_ast(join, from_ast);
    });
});
