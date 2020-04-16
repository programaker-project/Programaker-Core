import { AtomicFlowBlockData, BLOCK_TYPE as ATOMIC_BLOCK_TYPE } from '../../../flow-editor/atomic_flow_block';
import { BLOCK_TYPE as VALUE_BLOCK_TYPE, DirectValueFlowBlockData } from '../../../flow-editor/direct_value';
import { BLOCK_TYPE as ENUM_BLOCK_TYPE, EnumDirectValueFlowBlockData } from '../../../flow-editor/enum_direct_value';
import { FlowGraph } from '../../../flow-editor/flow_graph';

export function convert_to_graphviz(graph: FlowGraph): string {
    const tokens: string[] = ['digraph {'];
    const raws: {[key: string]: string} = {}

    let next_raw_id = 100000;
    let raw_value_prefixes = '__RAW_VALUE_';

    for (const node_id of Object.keys(graph.nodes)) {
        const node = graph.nodes[node_id];

        if (node.data.type === ATOMIC_BLOCK_TYPE) {
            const a_node = node.data as AtomicFlowBlockData;

            let fillcolor = "#ffffff";
            let fontcolor = "#000000";
            switch (a_node.value.options.type) {
                case 'operation':
                    fillcolor = '#aaaaff';
                    break;
                case 'getter':
                    fillcolor = '#aaffaa';
                    break;
                case 'trigger':
                    fillcolor = '#ffffaa';
                    break;
            }

            tokens.push(`"${node_id}"[label="${a_node.value.options.block_function}",`
                        +`style="filled",shape=rect,fontcolor="${fontcolor}",fillcolor="${fillcolor}"]`);
        }
        else if (node.data.type === VALUE_BLOCK_TYPE ) {
            const v_node = node.data as DirectValueFlowBlockData;
            raws[node_id] = v_node.value.value;
        }
        else if (node.data.type === ENUM_BLOCK_TYPE ) {
            const e_node = node.data as EnumDirectValueFlowBlockData;
            raws[node_id] = e_node.value.value_text;
        }
    }

    for (const conn of graph.edges) {
        let from_id = conn.from.id;
        if (raws[from_id]) {
            const value = raws[from_id];

            from_id = raw_value_prefixes + next_raw_id++;
            tokens.push(`"${from_id}"[label="${value}"]`);
        }
        tokens.push(`"${from_id}" -> "${conn.to.id}"[label="${conn.from.output_index} → ${conn.to.input_index}"];`);
    }

    tokens.push('}');
    return tokens.join('\n');
}
