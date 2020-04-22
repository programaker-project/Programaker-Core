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
    const trigger = builder.add_trigger('trigger_on_signal', {args: [[source, 0]]});

    const make_space = builder.add_op('op_delete_list_entry', { slots: { list: 'latest' }, args: [ 1 ] })

    const add_new = builder.add_op('op_add_to_list', { slots: { list: 'latest' }, args:  [ [source, 0] ] })
    make_space.then(add_new);

    const take_cond = builder.add_if(make_space, add_new, {
        cond: [f => f.add_getter('flow_greater_than',
                                 { args: [
                                     [
                                         f => f.add_getter('flow_list_length',
                                                           {slots: { list: 'latest' }}),
                                         0 ],
                                     3]
                                 }),
               0,
              ]
    });
    trigger.then_id(take_cond)

    const graph = builder.build();
    return graph;
}

describe('Flow-22: [Reactive] Take N last values.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });


    it('Should be able to compile', async () => {
        const compiled_flow = compile(gen_flow());

        const dsl_ast = dsl_to_ast(
            `;PM-DSL ;; Entrypoint for mmm-mode
            (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
            (if (> (list-length counter) 3)
                ((delete-list-index latest 1)))
            (add-to-list latest (flow-last-value "source" 0))
            `
        );

        const from_ast = [gen_compiled(dsl_ast)];

        are_equivalent_ast(compiled_flow, from_ast);
    });
});
