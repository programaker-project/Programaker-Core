import { AtomicFlowBlock } from './atomic_flow_block';
import { FlowBlock, Position2D } from './flow_block';
import { BlockExhibitor } from './block_exhibitor';
import { BlockManager } from './block_manager';
import { FlowWorkspace } from './flow_workspace';

export type BlockGenerator = (manager: BlockManager) => FlowBlock;

export class Toolbox {
    baseElement: HTMLElement;
    toolboxDiv: HTMLDivElement;
    blockShowcase: HTMLDivElement;
    workspace: FlowWorkspace;

    public static BuildOn(baseElement: HTMLElement, workspace: FlowWorkspace): Toolbox {
        let toolbox: Toolbox;
        try {
            toolbox = new Toolbox(baseElement, workspace);
            toolbox.init();
        }
        catch(err) {
            toolbox.dispose();

            throw err;
        }

        return toolbox;
    }


    private constructor(baseElement: HTMLElement, workspace: FlowWorkspace) {
        this.baseElement = baseElement;
        this.workspace = workspace;
    }

    onResize() {}

    dispose() {
        this.baseElement.removeChild(this.toolboxDiv);
    }

    init() {
        this.toolboxDiv = document.createElement('div');
        this.toolboxDiv.setAttribute('class', 'toolbox');
        this.baseElement.appendChild(this.toolboxDiv);

        this.blockShowcase = document.createElement('div');
        this.blockShowcase.setAttribute('class', 'showcase');
        this.toolboxDiv.appendChild(this.blockShowcase);
    }

    addBlockGenerator(generator: BlockGenerator) {
        const block_exhibitor = BlockExhibitor.FromGenerator(generator, this.blockShowcase);
        const element = block_exhibitor.getElement();
        element.onmousedown = (ev: MouseEvent) => {
            const rect = block_exhibitor.getInnerElementRect();

            if (!rect) {
                // Hidden block, ignore
                return;
            }

            const block = generator(this.workspace);
            element.classList.add('hidden');
            this.toolboxDiv.classList.add('subsumed');

            this.workspace.drawAbsolute(block, rect);
            (this.workspace as any)._mouseDownOnBlock(ev, block, () => {
                element.classList.remove('hidden');
                this.toolboxDiv.classList.remove('subsumed');
            });
        };
    }
}

export function buildBaseToolbox(baseElement: HTMLElement, workspace: FlowWorkspace): Toolbox {
    const tb = Toolbox.BuildOn(baseElement, workspace);
    tb.addBlockGenerator((manager) => {
        return new AtomicFlowBlock({
            message: 'wait',
            type: 'operation',
            inputs: [
                {
                    name: "seconds to wait",
                    type: "integer",
                },
            ],
            on_io_selected: manager.onIoSelected.bind(manager),
        });
    });

    return tb;
}
