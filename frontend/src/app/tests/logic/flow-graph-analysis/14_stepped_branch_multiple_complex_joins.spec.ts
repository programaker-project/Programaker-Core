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
    const cond = builder.add_stream('operator_equals', {args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});
    const branch1 = builder.add_op('control_wait', { id: 'branch1',
                                                        args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('control_wait', { id: 'branch2',
                                                        args: [ 2 ]
                                                      });

    const branch3 = builder.add_op('control_wait', { id: 'branch3',
                                                        args: [ 3 ]
                                                      });

    const branch4 = builder.add_op('control_wait', { id: 'branch4',
                                                        args: [ 4 ]
                                                      });

    const branch5 = builder.add_op('control_wait', { id: 'branch5',
                                                        args: [ 5 ]
                                                      });

    const branch6 = builder.add_op('control_wait', { id: 'branch6',
                                                        args: [ 6 ]
                                                      });

    builder.add_fork(trigger, [branch1, branch2, branch3, branch4, branch5, branch6 ]);

    // Join branch 1 and 2
    const joiner12 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch1, 'pulse' ], [branch2, 'pulse']]});
    joiner12.then(f => f.add_op('control_wait', { id: 'joiner12', args: [ 11 ] }));

    // Join branch (4 and 5), then operate, then merge with with 3
    const joiner45 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch4, 'pulse' ], [branch5, 'pulse']]});
    const branch4_bot = joiner45.then(f => f.add_op('control_wait', { id: 'branch4-5_bot',
                                                                         args: [ 4.5 ] }));

    // Join result of merge 4-5 with branch3
    const joiner34 = builder.add_trigger('trigger_when_all_completed', {args: [[ branch3, 'pulse' ], [branch4_bot, 'pulse']]});
    joiner34.then(f => f.add_op('control_wait', { id: 'joiner34', args: [ 12 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-14: Stepped branch with multiple, complex joins.', () => {
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
                ((fork
                  ;; Branches 1 and 2
                  ((fork
                    (control_wait 1) ; Branch 1
                    (control_wait 2)) ; Branch 2
                   ;; After branch 1 and 2 join
                   (control_wait 11))
                  ;; Branches 3, 4, 5
                  ((fork
                    (control_wait 3) ; Branch 3
                    ((fork ; Branches 4 and 5
                      (control_wait 4)
                      (control_wait 5))
                     (control_wait 4.5))) ; Join 4 and 5
                   ;; Join 3 and 4
                   (control_wait 12))
                  ;; Branch 6
                  (control_wait 6))
                 ))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
