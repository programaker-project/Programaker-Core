import { FlowGraph } from '../../../flow-editor/flow_graph';
import { validate } from '../../../flow-editor/graph_validation';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';

function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('camera1', { namespace: 'tts',
                                                   id: 'source', message: 'UTC time'});
    const sub = builder.add_stream('sub', { namespace: 'tts',
                                            args: [[source, 0], ['detector', 0]]});
    const noise_detector = builder.add_stream('noise_detection', { namespace: 'tts',
                                                                   id: 'detector', args: [[sub, 0]]});

    // Stepped section
    const trigger = builder.add_trigger('on_word_detection', { namespace: 'tts',
                                                               id: 'trigger', args: [[noise_detector, 0]]});
    const op = builder.add_op('show_string', { args: [[trigger, 1]]
                                             });

    trigger.then(op);

    const graph = builder.build();
    return graph;
}

describe('Flow-07: Streaming loops.', () => {
    it('Validation should FAIL', async () => {
        expect(() => validate(gen_flow()))
            .toThrowError(/ValidationError:.*loops? in streaming section.*/i);
    });
});
