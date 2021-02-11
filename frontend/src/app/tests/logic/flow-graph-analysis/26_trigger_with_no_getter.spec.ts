import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile, get_unreachable } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Services
    const chat = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Stepped section
    const trigger = builder.add_trigger('on_new_message', {id: 'trigger', namespace: chat, args: []});
    const operation = builder.add_op('respond', { namespace: chat,
                                                  args: [[ trigger, 1]]
                                                });
    trigger.then(operation);

    const graph = builder.build();
    return graph;
}

describe('Flow-26: Trigger with no getter.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should find no unreachable blocks', async () => {
        expect(get_unreachable(gen_flow())).toEqual([]);
    });

    it('Should be able to compile', async () => {
        const CHAT_SVC = "de5baefb-13da-457e-90a5-57a753da8891";

        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                 (services.de5baefb-13da-457e-90a5-57a753da8891.on_new_message )
                 (call-service id: ${CHAT_SVC}
                               action: respond
                               values: ((flow-last-value trigger 1)
                                        ))
                `))
        ]);
    });
});
