import { FlowGraph } from '../../../flow-editor/flow_graph';
import { validate } from '../../../flow-editor/graph_validation';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { compile } from '../../../flow-editor/graph_analysis';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { are_equivalent_ast } from './utils.spec';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const cond = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});

    const if_true = builder.add_op('op_wait_seconds', { args: [ 1 ] });
    const if_false = builder.add_op('op_wait_seconds', { args: [ 2 ] });
    trigger.then_id(builder.add_if(if_true, if_false, { cond: [cond, 0] }))

    const branch3 = builder.add_op('op_wait_seconds', { args: [ 3 ]
                                                      });

    const branch4 = builder.add_op('op_wait_seconds', { id: 'branch2',
                                                        args: [ 4 ]
                                                      });

    builder.add_fork(if_true, [branch3, branch4]);

    // Join branch 1 and 2
    const if_joiner = builder.add_trigger('trigger_when_first_completed', {args: [[branch3, 'pulse'], [branch4, 'pulse'], [ if_false, 'pulse']]});
    if_joiner.then(f => f.add_op('op_wait_seconds', { args: [ 9 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-21-08: IF then fork, close with IF merging (wait FIRST, short form).', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const compiled_flow = compile(gen_flow());

        const dsl_ast = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (if (and (= (flow-last-value "source" 0)
                        11)
                     )
                ((if (= (flow-last-value "source" 0)
                        11)
                     ((wait-seconds 1)
                      (fork :exit-when-first-completed
                       ((wait-seconds 3))
                       ((wait-seconds 4))))
                   ((wait-seconds 2)))
                 (wait-seconds 9)))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
