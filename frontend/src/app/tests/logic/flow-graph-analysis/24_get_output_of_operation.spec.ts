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

    // Stepped section
    const operation = builder.add_op('create_issue', { id: 'operation-with-value',
                                                       namespace: 'gitlab',
                                                       args: [ "Sample project", "Sample title" ]
                                                     });
    trigger.then(operation);
    operation
        .then(f => f.add_op('control_wait', { args: [ 1 ] }))
        .then(f => f.add_op('logging_add_log', { args: [[operation, 1]] }));

    const graph = builder.build();
    return graph;
}

describe('Flow-24: Get output of operation block.', () => {
    it('Validation should pass', async () => {
        expect(validate(gen_flow()))
            .toBeTruthy()
    });

    it('Should be able to compile', async () => {
        const TIME_BLOCK = "ad97e5d1-c725-4cc6-826f-30057f239635";
        const OP_BLOCK_ID = "operation-with-value";

        are_equivalent_ast(compile(gen_flow({ source_id: TIME_BLOCK })), [
            gen_compiled(dsl_to_ast(
                `;PM-DSL ;; Entrypoint for mmm-mode
                 (wait-for-monitor from_service: "${TIME_MONITOR_ID}")
                 (call-service id: "gitlab"
                                action: "create_issue"
                                values: ("Sample project" "Sample title"))
                 (wait-seconds 1)
                 (log (flow-last-value "${OP_BLOCK_ID}" 1))
                `))
        ]);
    });
});
