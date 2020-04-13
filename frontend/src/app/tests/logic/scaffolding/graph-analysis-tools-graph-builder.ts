import { AtomicFlowBlockData, AtomicFlowBlockOptions, BLOCK_TYPE as ATOMIC_BLOCK_TYPE, AtomicFlowBlock, AtomicFlowBlockType, AtomicFlowBlockOperationType } from '../../../flow-editor/atomic_flow_block';
import { BaseToolboxDescription, ToolboxDescription } from '../../../flow-editor/base_toolbox_description';
import { BLOCK_TYPE as VALUE_BLOCK_TYPE, DirectValueFlowBlockData } from '../../../flow-editor/direct_value';
import { BLOCK_TYPE as ENUM_BLOCK_TYPE, EnumDirectValueFlowBlockData, EnumDirectValueOptions } from '../../../flow-editor/enum_direct_value';
import { MessageType } from '../../../flow-editor/flow_block';
import { FlowGraph, FlowGraphEdge, FlowGraphNode } from '../../../flow-editor/flow_graph';
import { uuidv4 } from '../../../flow-editor/utils';


type NodeDescription = AtomicFlowBlockData | EnumDirectValueFlowBlockData | DirectValueFlowBlockData;

type ValueNodeRef = [string, number];
type StreamGenerator = (builder: GraphBuilder) => StreamNodeBuilderRef;
type NodeGenerator = (builder: GraphBuilder) => OpNodeBuilderRef;

type NodeRef = ValueNodeRef | StreamNodeBuilderRef | OpNodeBuilderRef;

type BlockArgument = [NodeRef, number] | [StreamGenerator, number] | ValueNodeRef | string | number;


function index_toolbox_description(desc: ToolboxDescription): {[key: string]: AtomicFlowBlockOptions} {
    const result: {[key: string]: AtomicFlowBlockOptions} = {};

    for (const cat of desc) {
        for (const block of cat.blocks) {
            result[block.block_function] = block;
        }
    }

    return result;
}

function infer_block_options(block_type: string,
                             options: BlockOptions,
                             params: { type: AtomicFlowBlockOperationType }): AtomicFlowBlockOptions {

    const inputs = [];
    for (const _arg of options.args || []) {
        inputs.push({ type: 'any' });
    }

    return {
        message: block_type,
        block_function: block_type,
        type: params.type,
        inputs: inputs,
        outputs: [
            {
                type: 'any',
            }
        ]
    };
}


const BaseBlocks = index_toolbox_description(BaseToolboxDescription);

interface BlockOptions {
    namespace?: string,
    message?: string,
    args?: BlockArgument[],
    id?: string,
}

class StreamNodeBuilderRef {
    builder: GraphBuilder;
    id: string;

    constructor(builder: GraphBuilder, id: string) {
        this.builder = builder;
        this.id = id;
    }
}

class OpNodeBuilderRef {
    builder: GraphBuilder;
    id: string;

    constructor(builder: GraphBuilder, id: string) {
        this.builder = builder;
        this.id = id;
    }

    then(next: NodeGenerator | OpNodeBuilderRef): OpNodeBuilderRef {
        if ((next as OpNodeBuilderRef).id) {
            const nextOp = (next as OpNodeBuilderRef);
            this.builder.establish_connection([this.id, 0], [nextOp.id, 0]);

            return nextOp;
        }
        else {
            const nextGen = next as NodeGenerator;
            const nextOp = nextGen(this.builder);
            this.builder.establish_connection([this.id, 0], [nextOp.id, 0]);

            return nextOp;
        }
    }
}

export class GraphBuilder {
    nodes: {[key: string]: NodeDescription} = {};
    edges: FlowGraphEdge[] = [];
    blocks: {[key: string]: AtomicFlowBlockOptions} = {};

    constructor() {
        this.blocks = BaseBlocks;
    }

    add_service(service_id?: string): string {
        if (!service_id) {
            service_id = uuidv4();
        }

        return service_id;
    }


    add_enum_node(namespace: string, name: string, value: string, id: string): ValueNodeRef {
        const ref = name + '_' + uuidv4();

        this.nodes[ref] = {
            type: ENUM_BLOCK_TYPE,
            value: {
                value_id: id,
                value_text: value,
                options: {
                    enum_name: name,
                    enum_namespace: namespace,
                } as EnumDirectValueOptions
            }
        }

        return [ref, 0];
    }

    private add_direct_node(value: any): ValueNodeRef {
        const ref = uuidv4();

        let type: MessageType = 'any';
        if (typeof value === 'string') {
            type = 'string';
        }
        else if (typeof value === 'number') {
            if (value % 1 === 0) {
                type = 'integer';
            }
            else {
                type = 'float';
            }
        }

        this.nodes[ref] = {
            type: VALUE_BLOCK_TYPE,
            value: {
                value: value,
                type: type,
            }
        };

        return [ref, 0];

    }

    private resolve_args(node: string, options: BlockOptions, offset?: number) {
        if (options.args) {
            let idx = -1;

            if (offset) {
                idx += offset;
            }

            for (const arg of options.args) {
                idx++;

                if (!arg) { continue; } // Skip nulls

                // Direct value
                if (typeof arg === 'number' || typeof arg === 'string') {
                    const ref = this.add_direct_node(arg);
                    this.establish_connection(ref, [node, idx]);
                }
                // Direct node reference
                else if (typeof (arg[0]) === 'string') {
                    this.establish_connection(arg as [string, number], [node, idx]);
                }
                // Node reference
                else if ((arg[0] as StreamNodeBuilderRef).id) {
                    this.establish_connection([(arg[0] as StreamNodeBuilderRef).id, arg[1]], [node, idx]);
                }
                // Generator
                else {
                    const gen = arg[0] as StreamGenerator;

                    this.establish_connection([gen(this).id, arg[1]], [node, idx]);
                }
            }
        }
    }

    add_stream(block_type: string, options?: BlockOptions): StreamNodeBuilderRef {
        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        const block_options = this.blocks[block_type];
        if (!block_options) {
            throw new Error(`Unknown block type: ${block_type}`);
        }

        const synth_in = AtomicFlowBlock.required_synth_inputs(block_options);

        this.nodes[ref] = {
            type: ATOMIC_BLOCK_TYPE,
            value: {
                options: block_options,
                synthetic_input_count: synth_in,
                synthetic_output_count: AtomicFlowBlock.required_synth_outputs(block_options),
            }
        }

        this.resolve_args(ref, options, synth_in);

        return new StreamNodeBuilderRef(this, ref);
    }

    add_trigger(block_type: string, options?: BlockOptions): OpNodeBuilderRef {
        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        const block_options = this.blocks[block_type];
        if (!block_options) {
            throw new Error(`Unknown block type: ${block_type}`);
        }

        const synth_in = AtomicFlowBlock.required_synth_inputs(block_options);

        this.nodes[ref] = {
            type: ATOMIC_BLOCK_TYPE,
            value: {
                options: block_options,
                synthetic_input_count: synth_in,
                synthetic_output_count: AtomicFlowBlock.required_synth_outputs(block_options),
            }
        }

        this.resolve_args(ref, options, synth_in);

        return new OpNodeBuilderRef(this, ref);
    }

    add_getter(block_type: string, options?: BlockOptions): StreamNodeBuilderRef {
        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        let block_options = this.blocks[block_type];

        if (!block_options) {
            block_options = infer_block_options(block_type, options, { type: 'getter' });
        }

        let synth_in: number, synth_out: number
        [block_options, synth_in, synth_out] = AtomicFlowBlock.add_synth_io(block_options);

        this.nodes[ref] = {
            type: ATOMIC_BLOCK_TYPE,
            value: {
                options: block_options,
                synthetic_input_count: synth_in,
                synthetic_output_count: synth_out,
            }
        }

        this.resolve_args(ref, options, synth_in);

        return new StreamNodeBuilderRef(this, ref);
    }

    add_op(block_type: string, options?: BlockOptions): OpNodeBuilderRef {
        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        let block_options = this.blocks[block_type];

        if (!block_options) {
            block_options = infer_block_options(block_type, options, { type: 'operation' });
        }

        let synth_in: number, synth_out: number
        [block_options, synth_in, synth_out] = AtomicFlowBlock.add_synth_io(block_options);

        this.nodes[ref] = {
            type: ATOMIC_BLOCK_TYPE,
            value: {
                options: block_options,
                synthetic_input_count: synth_in,
                synthetic_output_count: synth_out,
            }
        }

        this.resolve_args(ref, options, synth_in);

        return new OpNodeBuilderRef(this, ref);
    }

    establish_connection(source: [string, number], sink: [string, number]) {
        this.edges.push({
            from: { id: source[0], output_index: source[1] },
            to: { id: sink[0], input_index: sink[1] },
        });
    }

    build(): FlowGraph {
        const nodes: {[key: string]: FlowGraphNode} = {};

        for (const node_id of Object.keys(this.nodes)){
            const node = this.nodes[node_id];

            nodes[node_id] = {data: node, position: null};
        }

        return { nodes: nodes,
                 edges: this.edges,
               };

    }
}
