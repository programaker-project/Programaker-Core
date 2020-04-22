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
type VariableRef = {from_variable: string};

type BlockArgument = [OpNodeBuilderRef, 'pulse']
    | [NodeRef, number]
    | [StreamGenerator, number]
    | ValueNodeRef
    | VariableRef
    | string
    | number
;


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
        block_function: `services.${options.namespace}.${block_type}`,
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
    slots?: {[key: string]: string};
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

    then_id(next: string) {
        // Separated from `then` as this cannot guarantee that a
        // OpNodeBuilderRef that also can perform `.then` can be returned
        this.builder.establish_connection([this.id, 0], [next, 0]);
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


    add_enum_node(namespace: string, name: string, value: string, id: string, options?: { id: string }): ValueNodeRef {
        const ref = options && options.id ? options.id : 'enum_' + name + '_' + uuidv4();

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
        const ref = 'dir_' + value.toString().toLowerCase() + '_' + uuidv4();

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
                value: value + '',
                type: type,
            }
        };

        return [ref, 0];

    }

    private add_variable_getter_node(var_name: any): StreamNodeBuilderRef {
        const id = 'set_var_' + var_name + '_' + uuidv4();

        return this.add_getter('flow_get_var_value', { id: id, slots: { variable: var_name } });
    }

    private resolve_args(node: string, options?: BlockOptions, offset?: number) {
        if (options && options.args) {
            let idx = -1;

            if (offset) {
                idx += offset;
            }

            for (const arg of options.args) {
                idx++;

                if (arg === null || arg === undefined) { continue; }

                // Direct value
                if (typeof arg === 'number' || typeof arg === 'string') {
                    const ref = this.add_direct_node(arg);
                    this.establish_connection(ref, [node, idx]);
                }
                // Variaable reference
                else if ((arg as VariableRef).from_variable) {
                    const vblock = this.add_variable_getter_node((arg as VariableRef).from_variable);

                    this.establish_connection([ vblock.id, 0 ], [node, idx]);
                }
                // Direct node reference
                else if (typeof (arg[0]) === 'string') {
                    this.establish_connection(arg as [string, number], [node, idx]);
                }
                // Node reference
                else if ((arg[0] as StreamNodeBuilderRef).id) {
                    let out_index = arg[1] as number;
                    if (arg[1] === 'pulse'){
                        out_index = 0;
                    }
                    this.establish_connection([(arg[0] as StreamNodeBuilderRef).id, out_index], [node, idx]);
                }
                // Generator
                else {
                    let out_index = arg[1] as number;
                    if (arg[1] === 'pulse'){
                        out_index = 0;
                    }

                    const gen = arg[0] as StreamGenerator;

                    this.establish_connection([gen(this).id, out_index], [node, idx]);
                }
            }
        }
    }

    add_stream(block_type: string, options?: BlockOptions): StreamNodeBuilderRef {
        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        let block_options = this.blocks[block_type];
        if (!block_options) {
            if (options && options.namespace) {
                block_options = infer_block_options(block_type, options, { type: 'getter' });
            }
            else {
                throw new Error(`Unknown block type: ${block_type}`);
            }
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

    add_trigger(block_type: string, options?: BlockOptions): OpNodeBuilderRef {
        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        let block_options = this.blocks[block_type];
        if (!block_options) {
            if (options && options.namespace) {
                block_options = infer_block_options(block_type, options, { type: 'trigger' });
            }
            else {
                throw new Error(`Unknown block type: ${block_type}`);
            }
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

    add_getter(block_type: string, options?: BlockOptions): StreamNodeBuilderRef {
        const ref = options && options.id ? options.id : (block_type + '_' + uuidv4());

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

        if (options && options.slots) {
            block_options.slots = Object.assign(block_options.slots || {}, options.slots)
        }

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

    add_fork(source: OpNodeBuilderRef, branches: OpNodeBuilderRef[], options?: { id?: string }): string {
        if (!options) { options = {} };

        const block_type = 'op_fork_execution';

        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        let block_options = this.blocks[block_type];
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

        this.establish_connection([source.id, 0], [ref, 0]);
        let out_index = -1;
        for (const branch of branches) {
            out_index++;

            this.establish_connection([ref, out_index], [branch.id, 0]);
        }

        return ref;
    }

    add_if(if_true: OpNodeBuilderRef, if_false: OpNodeBuilderRef, options: { id?: string, cond: BlockArgument | StreamGenerator }): string {
        const block_type = 'control_if_else';

        const ref = options.id ? options.id : (block_type + '_' + uuidv4());

        let block_options = this.blocks[block_type];
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

        let cond = options.cond;
        if (typeof cond === 'function') {
            cond = [(cond as StreamGenerator)(this), 0];
        }

        this.resolve_args(ref, { args: [ cond ] }, synth_in);
        if (if_true) {
            this.establish_connection([ref, 0], [if_true.id, 0]);
        }
        if (if_false) {
            this.establish_connection([ref, 1], [if_false.id, 0]);
        }

        return ref;
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
