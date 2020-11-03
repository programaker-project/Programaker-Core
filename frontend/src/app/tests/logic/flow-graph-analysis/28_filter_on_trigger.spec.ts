import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Services
    const dataSource = builder.add_service('de5baefb-13da-457e-90a5-57a753da8891');

    // Trigger
    const trigger = builder.add_trigger('trigger_on_message', {id: 'trigger', args: [], namespace: dataSource});

    // Condition that flows from trigger
    const cond = builder.add_getter('operator_equals', {args: [[ trigger, 1], "/clear"]});
    const check = builder.add_op('control_if_else', {args: [[cond, 0]]});

    // Check result
    const log1 = builder.add_op('logging_add_log', { id: 'log1', args: ["Clearing"] });
    const log2 = builder.add_op('logging_add_log', { id: 'log2', args: ["Not clearing"] });

    trigger.then(check);
    check.then(log1, 0);
    check.then(log2, 1);

    const graph = builder.build();
    return graph;
}

describe('Flow-28: Filter trigger result.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const DATA_SVC = "de5baefb-13da-457e-90a5-57a753da8891";

        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (services.${DATA_SVC}.trigger_on_message)
                (if (= (flow-last-value "trigger" 1) "/clear")
                    ((log "Clearing"))
                  ((log "Not clearing")))
                `))
        ]);
    });
});
