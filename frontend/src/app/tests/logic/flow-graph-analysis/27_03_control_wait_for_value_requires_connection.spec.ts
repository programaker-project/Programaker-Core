import { FlowGraph } from '../../../flow-editor/flow_graph';
import { validate } from '../../../flow-editor/graph_validation';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Services
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Stepped section
    const trigger = builder.add_trigger('on_new_message', {id: 'trigger', namespace: chat, args: []});

    const operation = builder.add_op('control_wait_for_next_value', { args: []
                                                                    });
    trigger.then(operation);

    const graph = builder.build();
    return graph;
}

describe('Flow-27-03: Wait for value requires connection.', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError:.*Required input has no connections.*/i)
    });
});
