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

    // Stepped section
    const op = builder.add_op('logging_add_log', { args: [ [source, 0] ]
                                              });
    const cond = builder.add_if(op, null, {
        cond: [f => f.add_getter('operator_equals', { args: [ { from_variable: 'latest' },
                                                          [ f => f.add_getter('flow_get_thread_id'),
                                                            0 ]
                                                        ] }), 0]
    });

    builder
        .add_trigger('trigger_on_signal', {args: [[source, 0]]})
        .then(f => f.add_op('data_setvariableto', {
            args: [
                [f => f.add_getter('flow_get_thread_id'), 0],
            ],
            slots: { 'variable': 'latest' }
        }))
        .then(f => f.add_op('control_wait', {args: [ 1 ]}))
        .then_id(cond)
    ;

    const graph = builder.build();
    return graph;
}

describe('Flow-93: [Reactive] Debounce operation.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });


    it('Should be able to compile', async () => {
        const compiled_flow = compile(gen_flow());

        const dsl_ast = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (set-var latest (flow_get_thread_id))
            (wait-seconds 1)
            (if (= (get-var latest) (flow_get_thread_id))
                ((log (flow-last-value "source" "0"))))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
