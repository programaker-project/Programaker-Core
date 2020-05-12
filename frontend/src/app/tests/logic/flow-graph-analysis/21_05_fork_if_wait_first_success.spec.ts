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
    const cond = builder.add_stream('operator_equals', { id: 'eq-check', args: [[source, 0], 11]});

    // Stepped section
    const trigger = builder.add_trigger('trigger_when_all_true', {args: [[cond, 0]]});

    const branch1 = builder.add_op('control_wait', { id: 'branch1',
                                                        args: [ 1 ]
                                                      });

    const branch2 = builder.add_op('control_wait', { id: 'branch2',
                                                        args: [ 2 ]
                                                      });

    builder.add_fork(trigger, [branch1, branch2]);

    // Join branch 1 and 2
    const if_true = builder.add_op('control_wait', { args: [ 3 ] });
    const if_false = builder.add_op('control_wait', { args: [ 4 ] });

    branch2.then_id(builder.add_if(if_true, if_false, { cond: [cond, 0] }))


    const if_joiner = builder.add_trigger('trigger_when_first_completed', {args: [[ if_true, 'pulse' ], [if_false, 'pulse']]});
    const joiner = builder.add_trigger('trigger_when_first_completed', {args: [[branch1, 'pulse'], [ if_joiner, 'pulse']]});
    joiner.then(f => f.add_op('control_wait', { args: [ 9 ] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-21-05: Fork then IF, close merging FIRST (long form).', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const compiled_flow = compile(gen_flow());

        const dsl_ast = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor key: utc_time from_service: "${TIME_MONITOR_ID}")
            (if (and (= (flow-last-value "source" 0)
                        11)
                     )
                ((fork :exit-when-first-completed
                       ((wait-seconds 1))
                       ((wait-seconds 2)
                        (if (flow-last-value "eq-check" 0)
                            ((wait-seconds 3))
                          ((wait-seconds 4)))
                        ))
                 (wait-seconds 9)))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
