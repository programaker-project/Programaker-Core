import { FlowGraphNode, FlowGraph } from './flow-editor/flow_graph';
import { ProgramContent } from './program';

export class NoTranslationFoundError extends Error {}

export function getRequiredAssets(programData: ProgramContent): string[] {
    if (programData.type !== 'flow_program') {
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


        if (fun === 'command_call_service') {
            const origBridge = block.args.service_id;
            required[origBridge] = true;
        }
        else if ((!fun) || (!fun.startsWith('services.')) || (fun.startsWith('services.ui.'))) {
            continue;
        }
        else {
            const chunks = fun.split('.');

            const origBridge = chunks[1];
            required[origBridge] = true;
        }
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
        const parser = new DOMParser();
        const serializer = new XMLSerializer();

        const xml = parser.parseFromString(programData.orig, 'text/xml');
        const blocks = xml.getElementsByTagName("block");
        for (const block of Array.from(blocks)) {
            const fun = block.getAttribute("type");

            if ((!fun) || (!fun.startsWith('services.')) || (fun.startsWith('services.ui.'))) {
                continue;
            }

            const chunks = fun.split('.');

            const origBridge = chunks[1];
            required[origBridge] = true;
        }
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

function transformBlocks(blocks: any[], translations: {[key: string]: string}) {
    for (const block of blocks) {
        if (block.contents && block.contents.length > 0) {
            transformBlocks(block.contents, translations);
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
                transformBlocks(arg.value, translations);
            }
        }

        const fun = block.type;

        if (fun === 'command_call_service') {
            const origBridge = block.args.service_id;
            const translated = translations[origBridge];
            if (!translated) {
                throw new NoTranslationFoundError(origBridge)
            }

            console.debug(`[Program] On ${fun}: [${origBridge} -> ${translated}]`);
            block.args.service_id = translated;
        }
        else if ((!fun) || (!fun.startsWith('services.')) || (fun.startsWith('services.ui.'))) {
            continue;
        }
        else {
            const chunks = fun.split('.');

            const origBridge = chunks[1];
            const translated = translations[origBridge];
            if (!translated) {
                throw new NoTranslationFoundError(origBridge)
            }

            console.debug(`[Program] On ${fun}: [${origBridge} -> ${translated}]`);
            chunks[1] = translated;

            block.type = chunks.join('.');
        }
   }
}

// Destructively transform the program so it can be applied to the target user
export function transformProgram(programData: ProgramContent, translations: { [key: string]: string }) {

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
            const translated = translations[origBridge];
            if (!translated) {
                throw new Error(`Cannot find translation for bridge id=${origBridge}.`)
            }

            console.debug(`[Flow] On ${fun}: [${origBridge} -> ${translated}]`);
            chunks[1] = translated;
            node.data.value.options.block_function = chunks.join('.');
            if (node.data.value.icon) {
                node.data.value.icon.replace(origBridge, translated);
            }
        }
    }
    else if (programData.type === 'scratch_program') {
        const parser = new DOMParser();
        const serializer = new XMLSerializer();

        const xml = parser.parseFromString(programData.orig, 'text/xml');
        const blocks = xml.getElementsByTagName("block");
        for (const block of Array.from(blocks)) {
            const fun = block.getAttribute("type");

            if ((!fun) || (!fun.startsWith('services.')) || (fun.startsWith('services.ui.'))) {
                continue;
            }

            const chunks = fun.split('.');

            const origBridge = chunks[1];
            const translated = translations[origBridge];
            if (!translated) {
                throw new Error(`Cannot find translation for bridge id=${origBridge}.`)
            }

            chunks[1] = translated;
            block.setAttribute('type', chunks.join('.'));
        }
        programData.orig = serializer.serializeToString(xml);
    }
    else {
        throw new Error(`Translation of program type not supported: ${programData.type}`)
    }


    // Transform parsed structure
    // Maybe just recompiling the graph would be more robust
    try {
        for (const col of programData.parsed.blocks) {
            transformBlocks(col, translations);
        }
    }
    catch (err) {
        if (err instanceof NoTranslationFoundError) {
            const origBridge = err.message;
            throw new Error(`Cannot find translation for bridge id=${origBridge}.`);
        }
        throw err;
    }

}
