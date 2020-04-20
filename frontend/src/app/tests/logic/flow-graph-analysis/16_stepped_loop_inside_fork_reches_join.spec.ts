import { FlowGraph } from '../../../flow-editor/flow_graph';
import { compile } from '../../../flow-editor/graph_analysis';
import { validate } from '../../../flow-editor/graph_validation';
import { TIME_MONITOR_ID } from '../../../flow-editor/platform_facilities';
import { gen_compiled } from '../scaffolding/graph-analysis-tools';
import { dsl_to_ast } from '../scaffolding/graph-analysis-tools-ast-dsl';
import { GraphBuilder } from '../scaffolding/graph-analysis-tools-graph-builder';
import { are_equivalent_ast } from './utils.spec';

export function gen_flow(): FlowGraph {
    const builder = new GraphBuilder();

    // Stream section
    const source = builder.add_stream('flow_utc_time', {id: 'source', message: 'UTC time'});
    const cond = builder.add_stream('flow_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});
    const branch1 = builder.add_op('op_wait_seconds', { id: 'branch1',
                                                        args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('op_wait_seconds', { id: 'branch2',
                                                        args: [ 2 ]
                                                      });

    const branch3 = builder.add_op('op_wait_seconds', { id: 'branch3',
                                                        args: [ 3 ]
                                                      });


    const branch3_bot = builder.add_op('op_wait_seconds', { id: 'branch3-stepout',
                                                            args: [ 3.5 ]
                                                          });

    const branch3_cond = builder.add_if(branch3_bot, branch3,  { id: 'branch3-loop-helper',
                                                                 cond: f => builder.add_stream('flow_equals', {args: [1, 1]})
                                                               });

    branch3.then_id(branch3_cond);

    builder.add_fork(trigger, [branch1, branch2, branch3 ]);

    // Join branches
    const joiner = builder.add_trigger('trigger_when_all_completed', {args: [
        [ branch1, 'pulse' ],
        [ branch2, 'pulse' ],
        [ branch3_bot, 'pulse' ]
    ]});
    joiner.then(f => f.add_op('op_wait_seconds', { id: 'joiner', args: [ 11 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-16: Stepped loop inside fork reaches join.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });


    it('Should be able to compile', async () => {
        pending();

        const compiled_flow = compile(gen_flow());

        const dsl_ast = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (if (and (= (flow-last-value "source" 0)
                        11)
                     )
                ((fork
                  ;; Branches 1 and 2
                  ((fork
                    (op_wait_seconds 1) ; Branch 1
                    (op_wait_seconds 2))) ; Branch 2

                  ;; Branch 3
                  ((jump-point "loop-start")
                   (op_wait_seconds 3) ; Branch 3
                   (if (= 1 1)
                       ((op_wait_seconds 3.5))
                     ((jump-to "loop-start")))
                   )
                  )
                 (op_wait_seconds 11)))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
