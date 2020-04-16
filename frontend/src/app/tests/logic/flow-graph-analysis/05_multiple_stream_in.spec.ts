import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile, get_conversions_to_stepped, get_source_signals, get_unreachable } from '../../../flow-editor/graph_analysis';
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

    return graph;
}

describe('Flow-05: Multiple stream in.', () => {
    it('Validation should pass', async () => {
        expect(() => validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should find no unreachable blocks on samples/synth_05_multiple_streams', async () => {
        expect(get_unreachable(gen_flow())).toEqual([]);
    });

    it('Should be able to compile', async () => {
        const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";
        const WEATHER_SVC = "536bf266-fabf-44a6-ba89-a0c71b8db608";

        are_equivalent_ast(compile(gen_flow()),
                           [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                (if (and (= (flow-last-value "source1" 0)
                            11)
                         (= (flow-last-value "source2" 1)
                            (flow-last-value "source2" 2)
                            0))
                    ((call-service id: "${CHAT_SVC}"
                                   action: "send_message"
                                   values: ("-137414823"
                                            (call-service id: "${WEATHER_SVC}"
                                                          action: "get_today_max_in_place"
                                                          values: ("12/36/057/7"))))))
                `
            )),
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                (if (and (= (flow-last-value "source1" 0)
                            11)
                         (= (flow-last-value "source2" 1)
                            (flow-last-value "source2" 2)
                            0))
                    ((call-service id: "${CHAT_SVC}"
                                   action: "send_message"
                                   values: ("-137414823"
                                            (call-service id: "${WEATHER_SVC}"
                                                          action: "get_today_max_in_place"
                                                          values: ("12/36/057/7"))))))
                `
            ))
        ]);
    });

    describe('Intermediate step tests. ', async () => {
        it('Should recognize source signals', async () => {
            expect(get_source_signals(gen_flow())).toEqual([
                "source1",
                "source2",
            ]);
        });

        it('Should recognize conversions to stepped', async () => {
            expect(get_conversions_to_stepped(gen_flow(), "source1")).toEqual([
                "trigger",
            ]);
        });
    });
});
