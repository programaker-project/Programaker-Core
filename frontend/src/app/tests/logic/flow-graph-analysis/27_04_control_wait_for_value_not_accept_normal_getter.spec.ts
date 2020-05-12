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
    const test = builder.add_stream('operator_equals', {args: [0, 0]});

    const operation = builder.add_op('control_wait_for_next_value', { args: [[test, 0]]
                                                                    });
    trigger.then(operation);

    const graph = builder.build();
    return graph;
}

describe('Flow-27-04: Wait for value does not accept normal getter.', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/^ValidationError:.*Wait for value does not accept a getter as input.*/i)
    });
});
