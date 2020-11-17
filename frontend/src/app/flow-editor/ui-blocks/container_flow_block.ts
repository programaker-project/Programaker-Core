import { ContainerBlock, FlowBlockData, FlowBlockOptions, FlowBlock, Position2D } from '../flow_block';
import { FlowWorkspace } from '../flow_workspace';
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockOptions, UiFlowBlockData, isUiFlowBlockData, UiFlowBlockHandler } from './ui_flow_block';
import { UiSignalService } from 'app/services/ui-signal.service';
import { BlockManager } from '../block_manager';
import { Toolbox } from '../toolbox';

const SvgNS = "http://www.w3.org/2000/svg";

export type ContainerFlowBlockType = 'container_flow_block';
export const BLOCK_TYPE = 'container_flow_block';

const INPUT_PORT_REAL_SIZE = 10;
const OUTPUT_PORT_REAL_SIZE = 10;
const CONNECTOR_SIDE_SIZE = 15;
const ICON_PADDING = '1ex';

export interface ContainerFlowBlockBuilderInitOps {
    workspace?: FlowWorkspace,
}

export type ContainerTree = any; // TODO: Complete typing

export type GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => ContainerTree;

export interface ContainerFlowBlockOptions extends UiFlowBlockOptions {
    builder: UiFlowBlockBuilder,
    subtype: ContainerFlowBlockType,
    icon?: string,
    id: string,
    block_id?: string,

    gen_tree: GenTreeProc,
}

export interface ContainerFlowBlockData extends UiFlowBlockData {
    subtype: ContainerFlowBlockType,
}

export function isContainerFlowBlockOptions(opt: FlowBlockOptions): opt is ContainerFlowBlockOptions {
    return ((opt as ContainerFlowBlockOptions).subtype === BLOCK_TYPE);
}

export function isContainerFlowBlockData(data: FlowBlockData): data is ContainerFlowBlockData {
    return isUiFlowBlockData(data) && ((data as ContainerFlowBlockData).subtype === BLOCK_TYPE);
}

export class ContainerFlowBlock extends UiFlowBlock implements ContainerBlock {
    contents: FlowBlock[] = [];
    options: ContainerFlowBlockOptions;

    constructor(options: ContainerFlowBlockOptions,
                uiSignalService: UiSignalService,
               ) {
        super(options, uiSignalService);
    }

    addContentBlock(block: FlowBlock): void {
        this.contents.push(block);
    }

    removeContentBlock(block: FlowBlock): void {
        const pos = this.contents.findIndex(b => b === block);
        if (pos < 0) {
            throw new Error(`Block not found on container`);
        }

        this.contents.splice(pos, 1);
    }

    update(): void {

    }

    // private _reposition(): void {
    //     try {
    //         const endPositions = this.options.reposition(this.handler, this.contents.concat([]));

    //         let idx = 0;
    //         for (const block of this.contents) {
    //             const pos = block.getBodyArea();
    //             const tgt = endPositions[idx++];

    //             block.moveBy( {
    //                 x: pos.x - tgt.x,
    //                 y: pos.y - tgt.y,
    //             } );
    //         }
    //     }
    //     catch (err) {
    //         console.error(err);
    //     }
    // }

    gen_tree(): ContainerTree {
        return this.options.gen_tree(this.handler, this.contents.concat([]));
    }

    public static Deserialize(data: ContainerFlowBlockData, manager: BlockManager, toolbox: Toolbox): FlowBlock {
        if (data.subtype !== BLOCK_TYPE){
            throw new Error(`Block subtype mismatch, expected ${BLOCK_TYPE} found: ${data.subtype}`);
        }

        const options: ContainerFlowBlockOptions = JSON.parse(JSON.stringify(data.value.options));
        options.on_dropdown_extended = manager.onDropdownExtended.bind(manager);
        options.on_inputs_changed = manager.onInputsChanged.bind(manager);
        options.on_io_selected = manager.onIoSelected.bind(manager);

        const templateOptions = this._findTemplateOptions(options.id, toolbox)  as ContainerFlowBlockOptions;
        options.builder = templateOptions.builder;
        options.gen_tree = templateOptions.gen_tree;

        const block = new ContainerFlowBlock(options, toolbox.uiSignalService);

        if (data.value.dimensions) {
            block.blockData.dimensions = Object.assign({}, data.value.dimensions);
        }

        return block;
    }


    serialize(): FlowBlockData {
        return Object.assign(super.serialize(), { subtype: BLOCK_TYPE });
    }
}
