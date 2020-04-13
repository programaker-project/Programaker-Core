import { AtomicFlowBlockData, BLOCK_TYPE as ATOMIC_BLOCK_TYPE } from '../../../flow-editor/atomic_flow_block';
import { BLOCK_TYPE as VALUE_BLOCK_TYPE, DirectValueFlowBlockData } from '../../../flow-editor/direct_value';
import { BLOCK_TYPE as ENUM_BLOCK_TYPE, EnumDirectValueFlowBlockData } from '../../../flow-editor/enum_direct_value';
import { FlowGraph } from '../../../flow-editor/flow_graph';

export function convert_to_graphviz(graph: FlowGraph): string {
    const tokens: string[] = ['digraph {'];

    for (const node_id of Object.keys(graph.nodes)) {
        const node = graph.nodes[node_id];

        if (node.data.type === ATOMIC_BLOCK_TYPE) {
            const a_node = node.data as AtomicFlowBlockData;
            tokens.push(`"${node_id}"[label="${a_node.value.options.block_function}"]`);
        }
        else if (node.data.type === VALUE_BLOCK_TYPE ) {
            const v_node = node.data as DirectValueFlowBlockData;

            tokens.push(`"${node_id}"[label="${v_node.value.value}"]`);
        }
        else if (node.data.type === ENUM_BLOCK_TYPE ) {
            const e_node = node.data as EnumDirectValueFlowBlockData;

            tokens.push(`"${node_id}"[label="${e_node.value.value_text}"];\n`);
        }
    }

    for (const conn of graph.edges) {
        tokens.push(`"${conn.from.id}" -> "${conn.to.id}"[label="${conn.from.output_index} → ${conn.to.input_index}"];`);
    }

    tokens.push('}');
    return tokens.join('\n');
}
