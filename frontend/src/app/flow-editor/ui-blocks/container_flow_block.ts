import { UiSignalService } from '../../services/ui-signal.service';
import { BlockManager } from '../block_manager';
import { Area2D, ContainerBlock, FlowBlock, FlowBlockData, FlowBlockOptions, Movement2D, Resizeable, Position2D } from '../flow_block';
import { FlowWorkspace } from '../flow_workspace';
import { Toolbox } from '../toolbox';
import { CutTree, UiElementWidgetType } from './renderers/ui_tree_repr';
import { isUiFlowBlockData, UiFlowBlock, UiFlowBlockBuilderInitOps, UiFlowBlockData, UiFlowBlockHandler, UiFlowBlockOptions } from './ui_flow_block';

export type ContainerFlowBlockType = 'container_flow_block';
export const BLOCK_TYPE = 'container_flow_block';

const PUSH_DOWN_MARGIN = 10;
const PUSH_RIGHT_MARGIN = 10;

export interface ContainerFlowBlockBuilderInitOps {
    workspace?: FlowWorkspace,
}

export type GenTreeProc = (handler: UiFlowBlockHandler, blocks: FlowBlock[]) => CutTree;

export type ContainerFlowBlockBuilder = (canvas: SVGElement,
                                         group: SVGElement,
                                         block: ContainerFlowBlock,
                                         service: UiSignalService,
                                         ops: UiFlowBlockBuilderInitOps) => ContainerFlowBlockHandler;

export interface ContainerFlowBlockHandler extends UiFlowBlockHandler, Resizeable {
    repositionContents(): void;
    dropOnEndMove(): Movement2D;
    getBodyElement(): SVGGraphicsElement;
    updateContainer(container: UiFlowBlock): void;
    onContentUpdate: (contents: FlowBlock[]) => void;

    onGetFocus(): void;
    onLoseFocus(): void;

    readonly container: ContainerFlowBlock;
    update(): void; // Notify the handler of a change on the properties of the block
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
                blockId: string,
                uiSignalService: UiSignalService,
               ) {
        super(options, blockId, uiSignalService);
    }

    addContentBlock(block: FlowBlock): void {
        if (block === this) {
            throw Error("Block cannot be it's own content");
        }

        if (block instanceof UiFlowBlock && (block.hasAncestor(this))) {
            throw Error("This would create a container ↻ content loop");
        }
        else if (block instanceof ContainerFlowBlock && (block.contents.indexOf(this) >= 0)) {
            throw Error("This would create a container ↻ content loop");
        }

        this.handler.onContentUpdate(this.contents.concat([block]));
        this.contents.push(block);
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
        return !!this.options.isPage;
    }

    get cannotBeMoved(): boolean {
        // Right now only the pages cannot be moved, but this might change in the future
        return this.isPage;
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

    public static Deserialize(data: ContainerFlowBlockData, blockId: string, manager: BlockManager, toolbox: Toolbox): FlowBlock {
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

        const block = new ContainerFlowBlock(options, blockId, toolbox.uiSignalService);
        block.blockData = Object.assign({}, data.value.extra);

        return block;
    }

    serialize(): FlowBlockData {
        return Object.assign(super.serialize(), { subtype: BLOCK_TYPE });
    }

    public getBodyArea(): Area2D {
        const rect = this.handler.getBodyElement().getBBox();
        return {
            x: this.position.x,
            y: this.position.y,
            width: rect.width,
            height: rect.height,
        }
    }

    public moveBy(distance: {x: number, y: number}) {

        const dragged = super.moveBy(distance);

        return dragged.concat(this.moveContents(distance));
    }

    public moveContents(distance: Position2D) {
        let result = this.contents.concat([]);
        for (const block of this.contents) {
            const dragged = block.moveBy(distance);
            if (dragged.length > 0) {
                result = result.concat(dragged);
            }
        }

        return result;
    }

    public endMove(): FlowBlock[] {
        const movement = this.handler.dropOnEndMove();
        return this.moveBy(movement);
    }

    onGetFocus() {
        this.handler.onGetFocus();
    }

    onLoseFocus() {
        this.handler.onLoseFocus();
    }

    // Container-related
    updateContainer(container: FlowBlock) {
        this.handler.updateContainer(container as (UiFlowBlock | null));
    }

    recursiveGetAllContents(): FlowBlock[] {
        let acc: FlowBlock[] = [];

        for (const content of this.contents) {
            if (content instanceof ContainerFlowBlock) {
                acc = acc.concat(content.recursiveGetAllContents());
            }
            acc.push(content);
        }

        return acc;
    }

    repositionContents(){
        this.handler.repositionContents();
    }
}
