import { FlowGraph } from '../../../flow-editor/flow_graph';
import { get_unreachable } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import * as _02_lone_block from '../samples/02_lone_block.js';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';

function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    builder.add_op('send_message', { id: 'lone', namespace: chat });

    const graph = builder.build();
    return graph;
}

describe('Flow-02: Lone block.', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^Validation error:.*Unreachable blocks?.*/g)
    });

    it('Should find an unreachable block', async () => {
        expect(get_unreachable(gen_flow())).toEqual([
            "lone"
        ]);
    });

    describe('Sample-based tests.', async () => {
        it('Should find an unreachable block', async () => {
            expect(get_unreachable(_02_lone_block as FlowGraph)).toEqual([
                "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c"
            ]);
        });
    });
});
