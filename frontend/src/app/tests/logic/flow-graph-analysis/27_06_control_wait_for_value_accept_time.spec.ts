import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Services
    // Stepped section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});

    const cond = builder.add_stream('operator_equals', {args: [[source, 0], 11]});
    const trigger = builder.add_trigger('trigger_when_all_true', {id: 'trigger', args: [[cond, 0]]})

    const waited = builder.add_stream('flow_utc_time', {id: 'intermediate', message: 'UTC time'});

    const operation = builder.add_op('control_wait_for_next_value', { args: [[ waited, 1]]
                                                                    });
    trigger.then(operation);

    const graph = builder.build();
    return graph;
}

describe('Flow-27-06: Wait for value might have direct connection from a time trigger.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        are_equivalent_ast(compile(gen_flow()), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                (wait-for-monitor key: utc_time from_service: "${TIME_MONITOR_ID}")
                (if (and (= (flow-last-value "source" 0)
                            11))
                    ((control_wait_for_next_value (wait-for-monitor key: utc_time from_service: "${TIME_MONITOR_ID}" ))))
                `))
        ]);
    });
});
