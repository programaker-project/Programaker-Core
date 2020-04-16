import { FlowGraph } from '../../../flow-editor/flow_graph';
import { get_unreachable } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import * as _03_no_start_pulse from '../samples/03_no_start_pulse.js';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';

function gen_flow(): FlowGraph {
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

describe('Flow-03: No start pulse.', () => {
    it('Should find an unreachable blocks', async () => {
        expect(get_unreachable(gen_flow())).toEqual([
            "not-started",
        ]);
    });

    describe('Sample-based tests.', async () => {
        it('Should find unreachable blocks', async () => {
            expect(get_unreachable(_03_no_start_pulse as FlowGraph)).toEqual([
                "4652b79c-603b-4add-9164-92508be43fdf",
                "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
            ]);
        });
    });
});
