import { CompiledFlowGraph } from '../../../flow-editor/flow_graph';
import { canonicalize_ast_list } from '../scaffolding/graph-analysis-tools';
import { decompile_to_dsl } from '../scaffolding/graph-analysis-tools-dsl-decompiler';

export function are_equivalent_ast(actual: CompiledFlowGraph[], expected: CompiledFlowGraph[]) {
    const expectationMsg = `
╭──────────╮
│  RESULT  │
╰──────────╯

${decompile_to_dsl(actual).join('\n;; Alternative\n')}

╭──────────╮
│ EXPECTED │
╰──────────╯

${decompile_to_dsl(expected).join('\n;; Alternative\n')}
`;

    expect(canonicalize_ast_list(actual))
        .withContext(expectationMsg)
        .toEqual(canonicalize_ast_list(expected), expectationMsg);
}
