import { FlowGraphNode, FlowGraph } from './flow-editor/flow_graph';
import { ProgramContent } from './program';

export class NoTranslationFoundError extends Error {}

function getBlockRequiredBridges(blocks: any[], required: {[key: string]: boolean}) {
    for (const block of blocks) {
        if (block.contents && block.contents.length > 0) {
            getBlockRequiredBridges(block.contents, required);
        }

        let args = [];

        if (block.args instanceof Array) {
            args = block.args;
        }
        else if (block.args['service_call_values']) {
            args = block.args['service_call_values'];
        }

        for (const arg of args || []) {
            if (arg.type === 'block') {
                getBlockRequiredBridges(arg.value, required);
            }
        }

        const fun = block.type;

        if ((!fun) || (!fun.startsWith('services.')) || (fun.startsWith('services.ui.'))) {
            continue;
        }

        const chunks = fun.split('.');

        const origBridge = chunks[1];
        required[origBridge] = true;
   }
}

// Destructively transform the program so it can be applied to the target user
export function getRequiredBridges(programData: ProgramContent): string[] {

    const required: {[key: string]: boolean} = {};

    // Transform editor representation
    if (programData.type === 'flow_program') {
        // Transform graph
        const graph: FlowGraph = programData.orig;
        for (const node_id of Object.keys(graph.nodes)) {
            const node = graph.nodes[node_id];

            // Only simple_flow_blocks have to be transformed
            if (node.data.type !== 'simple_flow_block') {
                continue;
            }
            const fun = node.data.value.options.block_function;

            if ((!fun) || (!fun.startsWith('services.')) || (fun.startsWith('services.ui.'))) {
                continue;
            }

            const chunks = fun.split('.');

            const origBridge = chunks[1];
            required[origBridge] = true;
        }
    }
    else if (programData.type === 'scratch_program') {
        throw new Error('Translation of Scratch programs not yet supported');
    }
    else {
        throw new Error(`Translation of program type not supported: ${programData.type}`)
    }


    // Transform parsed structure
    // Maybe just recompiling the graph would be more robust
    for (const col of programData.parsed.blocks) {
        getBlockRequiredBridges(col, required);
    }

    return Object.keys(required);
}

function getFlowBlockIds(node: FlowGraphNode): string[] {
    const ids = [];

    const extra = node.data.value.extra;
    if (extra) {
        if (extra.settings) {
            if (extra.settings.body) {
                if (extra.settings.body.image) {
                    if (extra.settings.body.image.id) {
                        ids.push(extra.settings.body.image.id);
                    }
                }
            }
        }
    }

    return ids;
}

export function getRequiredAssets(programData: any): string[] {
    console.log(programData.metadata.type);

    if (programData.metadata.type !== 'flow_program') {
        return [];
    }

    const assets: {[key: string]: boolean} = {};

    // Transform graph
    const graph: FlowGraph = programData.orig;
    for (const node_id of Object.keys(graph.nodes)) {
        const node = graph.nodes[node_id];

        // Only simple_flow_blocks have to be transformed
        if (node.data.type !== 'ui_flow_block') {
            continue;
        }

        const ids = getFlowBlockIds(node);

        for (const id of ids) {
            assets[id] = true;
        }
    }

    return Object.keys(assets);
}
