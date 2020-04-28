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

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: options.source_id, message: 'UTC time'});
    const trigger = builder.add_trigger('trigger_on_signal', {args: [[source, 0]]});

    // Var
    const xblock = builder.add_variable_getter_node('x', { id: 'x1' });
    const xblock2 = builder.add_variable_getter_node('x', { id: 'x2' });

    const addition1 = builder.add_op('flow_addition', { id: 'add1', args: [[xblock, 0], 1] });
    const addition2 = builder.add_op('flow_addition', { id: 'add2', args: [[addition1, 0], [xblock, 0]] });
    const log1 = builder.add_op('op_log_value', { id: 'log1', args: [[addition1, 0]] });
    const log2 = builder.add_op('op_log_value', { id: 'log2', args: [[addition2, 0]] });
    const log3 = builder.add_op('op_log_value', { id: 'log3', args: [[xblock2, 0]] });

    trigger.then(log1).then(log2).then(log3);

    const graph = builder.build();
    return graph;
}

describe('Flow-25-01: Defend against glitches.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const TIME_BLOCK = "ad97e5d1-c725-4cc6-826f-30057f239635";

        are_equivalent_ast(compile(gen_flow({ source_id: TIME_BLOCK })), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                 (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                 (log (+ (get-var x) 1))
                 (log (+ (flow-last-value "add1" 0) (flow-last-value "x1" 0)))
                 (log (get-var x))
                `))
        ]);
    });
});
