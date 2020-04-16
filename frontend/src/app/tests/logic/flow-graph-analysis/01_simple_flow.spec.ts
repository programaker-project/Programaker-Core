import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile, get_conversions_to_stepped, get_stepped_ast, get_tree_with_ends, get_unreachable } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import * as _01_simple_flow from '../samples/01_simple_flow.js';
import { gen_compiled, SimpleArrayAstOperation } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';


function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Services
    const weather = builder.add_service('536bf266-fabf-44a6-ba89-a0c71b8db608');
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Values
    const loc = builder.add_enum_node(weather, 'get_locations', 'Vigo', '12/36/057/7');
    const channel = builder.add_enum_node(chat, 'get_known_channels', 'Bot testing', '-137414823');

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: options.source_id, message: 'UTC time'});
    const cond1 = builder.add_stream('flow_equals', {args: [[source, 1], [source, 2], 0]});
    const cond2 = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

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

describe('Flow-01: Simple flow.', () => {
    it('Validation should pass', async () => {
        expect(() => validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should find no unreachable blocks', async () => {
        expect(get_unreachable(gen_flow())).toEqual([]);
    });

    it('Should recognize conversions to stepped', async () => {
        expect(get_conversions_to_stepped(gen_flow({source_id: 'source'}), "source")).toEqual([
            "trigger",
        ]);
    });

    it('Synthetic compilation matches DSL program', async () => {
        const TIME_BLOCK = "ad97e5d1-c725-4cc6-826f-30057f239635";
        const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";
        const WEATHER_SVC = "536bf266-fabf-44a6-ba89-a0c71b8db608";

        are_equivalent_ast(compile(gen_flow({ source_id: TIME_BLOCK })), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                 (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                 (if (and (= (flow-last-value "${TIME_BLOCK}" 0)
                             11)
                          (= (flow-last-value "${TIME_BLOCK}" 1)
                             (flow-last-value "${TIME_BLOCK}" 2)
                             0))
                     ((call-service id: "${CHAT_SVC}"
                                    action: "send_message"
                                    values: ("-137414823"
                                             (call-service id: "${WEATHER_SVC}"
                                                           action: "get_today_max_in_place"
                                                           values: ("12/36/057/7"))))))
                `))
        ]);
    });

    // Based on sample
    describe('Sample-based tests.', async () => {
        it('Sample should match DSL compilation', async () => {
            const TIME_BLOCK = "ad97e5d1-c725-4cc6-826f-30057f239635";
            const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";
            const WEATHER_SVC = "536bf266-fabf-44a6-ba89-a0c71b8db608";

            are_equivalent_ast(compile(_01_simple_flow as FlowGraph), [
                gen_compiled(dsl_to_ast(
                    `;PM-DSL ;; Entrypoint for mmm-mode
                     (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                     (if (and (= (flow-last-value "${TIME_BLOCK}" 0)
                                 11)
                              (= (flow-last-value "${TIME_BLOCK}" 1)
                                 (flow-last-value "${TIME_BLOCK}" 2)
                                 0))
                         ((call-service id: "${CHAT_SVC}"
                                        action: "send_message"
                                        values: ("-137414823"
                                                 (call-service id: "${WEATHER_SVC}"
                                                               action: "get_today_max_in_place"
                                                               values: ("12/36/057/7"))))))
                    `))
            ]);
        });

        it('Sample should match synthetic compilation', async () => {
            are_equivalent_ast(compile(_01_simple_flow as FlowGraph),
                               compile(gen_flow({source_id: 'ad97e5d1-c725-4cc6-826f-30057f239635'})));
        });

        // Intermediate tests based on sample, might be removed if they prove to
        // costly to maintain
        it('Should build correctly the stepper AST from sample', async () => {
            expect(get_stepped_ast(_01_simple_flow as FlowGraph, "032d2a4e-bfe2-4635-a1cf-dc62692eead7"))
                .toEqual([
                    {
                        block_id: "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
                        arguments: [
                            {
                                tree: {
                                    block_id: "f1b8670c-0001-4417-8a39-2c52f5140383",
                                    arguments: []
                                },
                                output_index: 0,
                            },
                            {
                                tree: {
                                    block_id: "918294c3-1e7d-4b2f-ab73-77188a4b89b0",
                                    arguments: [
                                        {
                                            tree: {
                                                block_id: "fc4bd63f-d4ff-4e15-8b09-a224e6e1c635",
                                                arguments: [],
                                            },
                                            output_index: 0
                                        }
                                    ]
                                },
                                output_index: 0,
                            }
                        ],
                        contents: []
                    }
                ]);
        });

        it('Sample should build correctly the streaming tree', async () => {
            expect(get_tree_with_ends(_01_simple_flow as FlowGraph,
                                      "ad97e5d1-c725-4cc6-826f-30057f239635",
                                      "032d2a4e-bfe2-4635-a1cf-dc62692eead7"))
                .toEqual({
                    block_id: "032d2a4e-bfe2-4635-a1cf-dc62692eead7",
                    arguments: [
                        { // Hour = 11
                            tree: {
                                block_id: "0f82640f-b40b-4053-981d-1fe0b2c17de0",
                                arguments: [
                                    { // Hour
                                        tree: {
                                            block_id: "ad97e5d1-c725-4cc6-826f-30057f239635",
                                            arguments: [],
                                        },
                                        output_index: 0
                                    },
                                    { // "11"
                                        tree: {
                                            block_id: "0b2f1836-aaf3-4e13-84b9-8041c3b5b4b8",
                                            arguments: [],
                                        },
                                        output_index: 0,
                                    }
                                ]
                            },
                            output_index: 0
                        },
                        { // Minute = Second = 0
                            tree: {
                                block_id: "0d4937a0-cdd3-4bf6-b8e8-8da2974b1330",
                                arguments: [
                                    {
                                        tree: {
                                            block_id: "ad97e5d1-c725-4cc6-826f-30057f239635",
                                            arguments: [],
                                        },
                                        output_index: 1
                                    },
                                    {
                                        tree: {
                                            block_id: "ad97e5d1-c725-4cc6-826f-30057f239635",
                                            arguments: [],
                                        },
                                        output_index: 2
                                    },
                                    {
                                        tree: {
                                            block_id: "2b3b54d4-bcfa-4137-825f-ca52e2be4e96",
                                            arguments: [],
                                        },
                                        output_index: 0,
                                    }
                                ]
                            },
                            output_index: 0
                        }
                    ]
                });
        });
    });

    // Same, but testing intermediate scaffoling, might be removed if too costly
    describe('Scaffold step testing.', async () => {
        it('Should match tool compilation', async () => {
            are_equivalent_ast(compile(_01_simple_flow as FlowGraph), [
                gen_compiled([
                    ['wait_for_monitor', { monitor_id: { from_service: TIME_MONITOR_ID }, expected_value: 'any_value' }],
                    ['control_if_else',
                     ['operator_and', ['operator_equals',
                                       ['flow_last_value', 'ad97e5d1-c725-4cc6-826f-30057f239635', 0],
                                       11],
                      ['operator_equals',
                       ['flow_last_value', 'ad97e5d1-c725-4cc6-826f-30057f239635', 1],
                       ['flow_last_value', 'ad97e5d1-c725-4cc6-826f-30057f239635', 2],
                       0
                      ]
                     ] as SimpleArrayAstOperation,
                     [
                         ['command_call_service', {
                             service_id: 'de5baefb-13da-457e-90a5-57a753da8891',
                             service_action: 'send_message',
                             service_call_values: ['-137414823',
                                                   ['command_call_service', {
                                                       service_id: '536bf266-fabf-44a6-ba89-a0c71b8db608',
                                                       service_action: 'get_today_max_in_place',
                                                       service_call_values: ["12/36/057/7"]
                                                   }]],
                         }]
                     ]
                    ]
                ])
            ]);
        });
    });

});
