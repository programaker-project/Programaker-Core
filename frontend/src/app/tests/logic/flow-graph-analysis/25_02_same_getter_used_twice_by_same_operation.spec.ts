import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { extract_internally_reused_arguments } from '../../../flow-editor/graph_transformations';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function process_flow(graph: FlowGraph): FlowGraph {
    // Used by visualizing script to produce a processed version specific for this test
    return extract_internally_reused_arguments(graph);
}

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: options.source_id, message: 'UTC time'});
    const trigger = builder.add_trigger('trigger_on_signal', {args: [[source, 0]]});

    // Var
    const xblock = builder.add_variable_getter_node('x', { id: 'x' });

    const addition1 = builder.add_op('operator_add', { id: 'add1', args: [[xblock, 0], 1] });
    const addition2 = builder.add_op('operator_add', { id: 'add2', args: [[xblock, 0], 2] });
    const join_add = builder.add_op('operator_add', { id: 'join_add', args: [[addition1, 0], [addition2, 0]] });
    const log1 = builder.add_op('logging_add_log', { id: 'log1', args: [[join_add, 0]] });

    trigger.then(log1);

    const graph = builder.build();
    return graph;
}

describe('Flow-25-02: Same getter used twice by same operation.', () => {
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
                 (preload (get-var x))
                 (log (+ (+ (flow-last-value x 0) 1)
                         (+ (flow-last-value x 0) 2)))
                `))
        ]);
    });
});
