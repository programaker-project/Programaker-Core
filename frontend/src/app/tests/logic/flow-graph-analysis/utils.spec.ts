import { CompiledFlowGraph } from '../../../flow-editor/flow_graph';
import { canonicalize_ast_list } from '../scaffolding/graph-analysis-tools';

export function are_equivalent_ast(actual: CompiledFlowGraph[], expected: CompiledFlowGraph[]) {
    return expect(canonicalize_ast_list(actual)).toEqual(canonicalize_ast_list(expected));
}
