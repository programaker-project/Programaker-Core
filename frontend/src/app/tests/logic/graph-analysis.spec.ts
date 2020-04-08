import { FlowGraph, CompiledFlowGraph } from '../../flow-editor/flow_graph';
import { TIME_MONITOR_ID } from '../../flow-editor/platform_facilities';
import { get_unreachable, compile } from '../../flow-editor/graph_analysis';
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

describe('FlowGraphAnalysis Compilation', () => {
    it('should compile samples/01_simple_flow', async () => {
        expect(compile(_01_simple_flow as FlowGraph)).toEqual(
            [
                {
                    type: "wait_for_monitor",
                    args: {
                        monitor_id: {
                            from_service: TIME_MONITOR_ID,
                        },
                        expected_value: 'any_value',
                    }
                },
                {
                    type: "control_if_else",
                    args: [
                        {
                            type: 'block',
                            content: [
                                {
                                    type: 'operator_and',
                                    args: [
                                        {
                                            type: 'block',
                                            content: [
                                                {
                                                    type: 'operator_equals',
                                                    args: [
                                                        {
                                                            type: 'block',
                                                            content: [
                                                                {
                                                                    type: 'flow_last_value',
                                                                    args: [
                                                                        {
                                                                            type: 'constant',
                                                                            value: 'ad97e5d1-c725-4cc6-826f-30057f239635',
                                                                        },
                                                                        {
                                                                            type: 'constant',
                                                                            value: 0,
                                                                        }
                                                                    ]
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            type: 'constant',
                                                            value: 11,
                                                        }
                                                    ]
                                                },
                                            ],
                                        },
                                        {
                                            type: 'block',
                                            content: [
                                                {
                                                    type: 'operator_equals',
                                                    args: [
                                                        {
                                                            type: 'block',
                                                            content: [
                                                                {
                                                                    type: 'flow_last_value',
                                                                    args: [
                                                                        {
                                                                            type: 'constant',
                                                                            value: 'ad97e5d1-c725-4cc6-826f-30057f239635',
                                                                        },
                                                                        {
                                                                            type: 'constant',
                                                                            value: 1,
                                                                        }
                                                                    ]
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            type: 'block',
                                                            content: [
                                                                {
                                                                    type: 'flow_last_value',
                                                                    args: [
                                                                        {
                                                                            type: 'constant',
                                                                            value: 'ad97e5d1-c725-4cc6-826f-30057f239635',
                                                                        },
                                                                        {
                                                                            type: 'constant',
                                                                            value: 2,
                                                                        }
                                                                    ]
                                                                }
                                                            ]
                                                        },
                                                        {
                                                            type: 'constant',
                                                            value: 0,
                                                        }
                                                    ]
                                                },
                                            ],
                                        }
                                    ]
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
                                                contents: [
                                                    {
                                                        type: "command_call_service",
                                                        args: {
                                                            service_id: '536bf266-fabf-44a6-ba89-a0c71b8db608',
                                                            service_action: 'get_today_max_in_place',
                                                            service_call_values: [
                                                                '12/36/057/7'
                                                            ]
                                                        }
                                                    }
                                                ]
                                            }
                                        ]
                                    }
                                }
                            ]
                        }
                    ]
                }
            ] as CompiledFlowGraph,
        );
    });
});
