import { ContainerBlock, FlowBlockData, FlowBlockOptions, FlowBlock, Position2D } from '../flow_block';
import { FlowWorkspace } from '../flow_workspace';
import { UiFlowBlock, UiFlowBlockBuilder, UiFlowBlockOptions, UiFlowBlockData, isUiFlowBlockData, UiFlowBlockHandler, UiFlowBlockBuilderInitOps } from './ui_flow_block';
import { UiSignalService } from 'app/services/ui-signal.service';
import { BlockManager } from '../block_manager';
import { Toolbox } from '../toolbox';
import { CutTree, UiElementWidgetType } from './renderers/ui_tree_repr';

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

export type GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => CutTree;

export type ContainerFlowBlockBuilder = (canvas: SVGElement,
                                  group: SVGElement,
                                  block: UiFlowBlock,
                                  service: UiSignalService,
                                  ops: UiFlowBlockBuilderInitOps) => ContainerFlowBlockHandler;

export interface ContainerFlowBlockHandler extends UiFlowBlockHandler {
    onContentUpdate: (contents: FlowBlock[]) => void,
}

export interface ContainerFlowBlockOptions extends UiFlowBlockOptions {
    builder: ContainerFlowBlockBuilder,
    subtype: ContainerFlowBlockType,
    icon?: string,
    id: UiElementWidgetType,
    block_id?: string,
    isPage: boolean,

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
    handler: ContainerFlowBlockHandler;

    constructor(options: ContainerFlowBlockOptions,
                uiSignalService: UiSignalService,
               ) {
        super(options, uiSignalService);
    }

    addContentBlock(block: FlowBlock): void {
        this.contents.push(block);
        this.handler.onContentUpdate(this.contents);
    }

    removeContentBlock(block: FlowBlock): void {
        const pos = this.contents.findIndex(b => b === block);
        if (pos < 0) {
            throw new Error(`Block not found on container`);
        }

        this.contents.splice(pos, 1);
        this.handler.onContentUpdate(this.contents);
    }

    update(): void {
        this.handler.onContentUpdate(this.contents);
    }

    get isPage(): boolean {
        return this.options.isPage;
    }

    getPageTitle(): string {
        if (!this.isPage) {
            return null;
        }

        if (!this.handler.isTextReadable()) {
            return null;
        }

        return this.handler.text;
    }

    public renderAsUiElement(): CutTree {
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
        block.blockData = Object.assign({}, data.value.extra);

        return block;
    }

    serialize(): FlowBlockData {
        return Object.assign(super.serialize(), { subtype: BLOCK_TYPE });
    }


    public moveBy(distance: {x: number, y: number}) {

        const dragged = super.moveBy(distance);
        let result = dragged.concat(this.contents);
        for (const block of this.contents) {
            const dragged = block.moveBy(distance);
            if (dragged.length > 0) {
                result = result.concat(dragged);
            }
        }

        return result;
    }
}
