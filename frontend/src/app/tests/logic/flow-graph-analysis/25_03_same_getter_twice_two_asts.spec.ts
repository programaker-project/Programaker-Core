import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { extract_internally_reused_argument } from '../../../flow-editor/graph_transformations';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function process_flow(graph: FlowGraph): FlowGraph {
    // Used by visualizing script to produce a processed version specific for this test
    return extract_internally_reused_argument(graph);
}

export function gen_flow(options?: { source_id?: string }): FlowGraph {
    if (!options) { options = {} }

    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: options.source_id, message: 'UTC time'});
    const trigger1 = builder.add_trigger('trigger_on_signal', {args: [[source, 0]]});
    const trigger2 = builder.add_trigger('trigger_on_signal', {args: [[source, 0]]});

    // Var
    const xblock = builder.add_variable_getter_node('x', { id: 'x' });

    const addition11 = builder.add_op('flow_addition', { id: 'add11', args: [[xblock, 0], 1] });
    const addition12 = builder.add_op('flow_addition', { id: 'add12', args: [[xblock, 0], 2] });
    const join_add1 = builder.add_op('flow_addition', { id: 'join_add1', args: [[addition11, 0], [addition12, 0]] });
    const log1 = builder.add_op('op_log_value', { id: 'log1', args: [[join_add1, 0]] });

    const addition21 = builder.add_op('flow_addition', { id: 'add21', args: [[xblock, 0], 1] });
    const addition22 = builder.add_op('flow_addition', { id: 'add22', args: [[xblock, 0], 2] });
    const join_add2 = builder.add_op('flow_addition', { id: 'join_add2', args: [[addition21, 0], [addition22, 0]] });
    const log2 = builder.add_op('op_log_value', { id: 'log2', args: [[join_add2, 0]] });

    trigger1.then(log1);
    trigger2.then(log2);

    const graph = builder.build();
    return graph;
}

describe('Flow-25-03: Same getter used twice in two ASTs.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const TIME_BLOCK = "ad97e5d1-c725-4cc6-826f-30057f239635";

        const expected_ast = gen_compiled(dsl_to_ast(`;PM-DSL;; Entrypoint for mmm-mode
             (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
             (preload (get-var x))
             (log (+ (+ (flow-last-value x 0) 1)
                     (+ (flow-last-value x 0) 2)))
            `));
        are_equivalent_ast(compile(gen_flow({ source_id: TIME_BLOCK })),
                           [ expected_ast, expected_ast ]);
    });
});
