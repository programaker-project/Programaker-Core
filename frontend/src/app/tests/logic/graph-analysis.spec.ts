import { FlowGraph } from '../../flow-editor/flow_graph';
import { TIME_MONITOR_ID } from '../../flow-editor/platform_facilities';
import { get_unreachable, compile, get_source_signals, get_conversions_to_stepped, get_tree_with_ends, get_stepped_ast } from '../../flow-editor/graph_analysis';
import * as _01_simple_flow from './samples/01_simple_flow.js';
import * as _02_lone_block from './samples/02_lone_block.js';
import * as _03_no_start_pulse from './samples/03_no_start_pulse.js';
import * as _04_no_start_loop from './samples/04_no_start_loop.js';

describe('FlowGraphAnalysis Reachability', () => {
    it('should find no unreachable blocks on samples/01_simple_flow', async () => {
        expect(get_unreachable(_01_simple_flow as FlowGraph)).toEqual([]);
    });

    it('should find an unreachable block on sample/02_lone_block', async () => {
        expect(get_unreachable(_02_lone_block as FlowGraph)).toEqual([
            "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c"
        ]);
    });

    it('should find unreachable blocks on sample/03_no_start_pulse', async () => {
        expect(get_unreachable(_03_no_start_pulse as FlowGraph)).toEqual([
            "4652b79c-603b-4add-9164-92508be43fdf",
            "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        ]);
    });

    it('should find unreachable blocks on sample/04_no_start_loop', async () => {
        expect(get_unreachable(_04_no_start_loop as FlowGraph)).toEqual([
            "4652b79c-603b-4add-9164-92508be43fdf",
            "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        ]);
    });
});

describe('FlowGraphAnalysis Compilation Steps', () => {
    it('should recognize source signals on samples/01_simple_flow', async () => {
        expect(get_source_signals(_01_simple_flow as FlowGraph)).toEqual([
            "ad97e5d1-c725-4cc6-826f-30057f239635",
        ]);
    });

    it('should recognize conversions to stepped on samples/01_simple_flow', async () => {
        expect(get_conversions_to_stepped(_01_simple_flow as FlowGraph, "ad97e5d1-c725-4cc6-826f-30057f239635")).toEqual([
            "032d2a4e-bfe2-4635-a1cf-dc62692eead7",
        ]);
    });

    it('should build correctly the streaming tree for samples/01_simple_flow', async () => {
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

    it('should build correctly the stepper AST on samples/01_simple_flow', async () => {
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
});

describe('FlowGraphAnalysis Compilation E2E', () => {
    it('should compile samples/01_simple_flow', async () => {
        expect(compile(_01_simple_flow as FlowGraph)).toEqual([
            [
                {
                    type: "wait_for_monitor",
                    args: {
                        monitor_id: {
                            from_service: TIME_MONITOR_ID,
                        },
                        expected_value: 'any_value',
                    },
                    contents: []
                },
                {
                    type: "control_if_else",
                    args: [
                        {
                            type: 'block',
                            value: [
                                {
                                    type: 'operator_and',
                                    args: [
                                        {
                                            type: 'block',
                                            value: [
                                                {
                                                    type: 'operator_equals',
                                                    args: [
                                                        {
                                                            type: 'block',
                                                            value: [
                                                                {
                                                                    type: 'flow_last_value',
                                                                    args: [
                                                                        {
                                                                            type: 'constant',
                                                                            value: 'ad97e5d1-c725-4cc6-826f-30057f239635',
                                                                        },
                                                                        {
                                                                            type: 'constant',
                                                                            value: '0',
                                                                        }
                                                                    ],
                                                                    contents: [],
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            type: 'constant',
                                                            value: '11',
                                                        }
                                                    ],
                                                    contents: []
                                                },
                                            ],
                                        },
                                        {
                                            type: 'block',
                                            value: [
                                                {
                                                    type: 'operator_equals',
                                                    args: [
                                                        {
                                                            type: 'block',
                                                            value: [
                                                                {
                                                                    type: 'flow_last_value',
                                                                    args: [
                                                                        {
                                                                            type: 'constant',
                                                                            value: 'ad97e5d1-c725-4cc6-826f-30057f239635',
                                                                        },
                                                                        {
                                                                            type: 'constant',
                                                                            value: '1',
                                                                        }
                                                                    ],
                                                                    contents: [],
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            type: 'block',
                                                            value: [
                                                                {
                                                                    type: 'flow_last_value',
                                                                    args: [
                                                                        {
                                                                            type: 'constant',
                                                                            value: 'ad97e5d1-c725-4cc6-826f-30057f239635',
                                                                        },
                                                                        {
                                                                            type: 'constant',
                                                                            value: '2',
                                                                        }
                                                                    ],
                                                                    contents: []
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            type: 'constant',
                                                            value: '0',
                                                        }
                                                    ],
                                                    contents: []
                                                },
                                            ],
                                        }
                                    ],
                                    contents: []
                                }
                            ]
                        }
                    ],
                    contents: [
                        {
                            contents: [
                                {
                                    type: "command_call_service",
                                    args: {
                                        service_id: 'de5baefb-13da-457e-90a5-57a753da8891',
                                        service_action: 'send_message',
                                        service_call_values: [
                                            {
                                                type: 'constant',
                                                value: '-137414823',
                                            },
                                            {
                                                type: 'block',
                                                value: [
                                                    {
                                                        type: "command_call_service",
                                                        args: {
                                                            service_id: '536bf266-fabf-44a6-ba89-a0c71b8db608',
                                                            service_action: 'get_today_max_in_place',
                                                            service_call_values: [
                                                                {
                                                                    type: "constant",
                                                                    value: "12/36/057/7"
                                                                }
                                                            ]
                                                        },
                                                        contents: []
                                                    }
                                                ]
                                            }
                                        ]
                                    },
                                    contents: []
                                }
                            ]
                        },
                        {
                            contents: []
                        }
                    ]
                }
            ]
        ]);
    });

});
