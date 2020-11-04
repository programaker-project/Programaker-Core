import { BlockExhibitor } from './block_exhibitor';
import { BlockManager } from './block_manager';
import { FlowBlock, Position2D, FlowBlockOptions } from './flow_block';
import { FlowWorkspace } from './flow_workspace';
import { UiFlowBlockOptions } from './ui-blocks/ui_flow_block';

export type BlockGenerator = (manager: BlockManager) => FlowBlock;

export class Toolbox {
    baseElement: HTMLElement;
    toolboxDiv: HTMLDivElement;
    blockShowcase: HTMLDivElement;
    workspace: FlowWorkspace;
    categories: { [key: string]: HTMLDivElement } = {};
    blocks: FlowBlockOptions[] = [];

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

    setCategory(cat:{ id: string, name: string }) {
        const [div, updated] = this.getOrCreateCategory(cat);
        if (!updated) {
            const title = div.getElementsByClassName('category_title')[0] as HTMLDivElement;
            title.innerText = cat.name;
        }
    }

    private getOrCreateCategory(cat:{ id: string, name: string }): [HTMLDivElement, boolean] {
        let category_div = this.categories[cat.id];
        let created_now = false;

        if (!category_div) {
            category_div = this.categories[cat.id] = document.createElement('div');
            category_div.setAttribute('class', 'category empty cat_name_' + cat.name + ' cat_id_' + cat.id);
            this.blockShowcase.appendChild(category_div);

            const cat_title = document.createElement('div');
            cat_title.setAttribute('class', 'category_title');
            cat_title.innerText = cat.name;
            category_div.appendChild(cat_title)
            cat_title.onclick = () => {
                if (category_div.classList.contains('collapsed')) {
                    category_div.classList.remove('collapsed');
                }
                else {
                    category_div.classList.add('collapsed');
                }
            };

            created_now = true;
        }

        return [category_div, created_now];
    }

    addBlockGenerator(generator: BlockGenerator, category_id: string) {
        const [category_div] = this.getOrCreateCategory({ id: category_id, name: category_id })
        category_div.classList.remove('empty');

        const block_exhibitor = BlockExhibitor.FromGenerator(generator, category_div);
        const element = block_exhibitor.getElement();
        element.onmousedown = element.ontouchstart = (ev: MouseEvent | TouchEvent) => {
            try {
                const rect = block_exhibitor.getInnerElementRect();

                if (!rect) {
                    // Hidden block, ignore
                    return;
                }

                const block = generator(this.workspace);
                element.classList.add('hidden');
                this.toolboxDiv.classList.add('subsumed');

                const block_id = this.workspace.drawAbsolute(block, rect);
                const pos = this.workspace._getPositionFromEvent(ev);

                (this.workspace as any)._mouseDownOnBlock(pos, block, (ev: Position2D) => {

                    element.classList.remove('hidden');
                    this.toolboxDiv.classList.remove('subsumed');

                    // Check if the block was dropped on the toolbox, if so remove it
                    const toolboxRect = this.toolboxDiv.getClientRects()[0];
                    if ((ev.x >= toolboxRect.x) && (ev.x <= toolboxRect.x + toolboxRect.width)) {
                        if ((ev.y >= toolboxRect.y) && (ev.y <= toolboxRect.y + toolboxRect.height)) {
                            // Dropped on toolbox
                            console.log("Dropped on toolbox, cleaning up");
                            this.workspace.removeBlock(block_id);
                        }
                    }

                });

                if ((ev as TouchEvent).targetTouches) {
                    // Redirect touch events to the canvas. If we don't do this,
                    // the canvas won't receive touchmove or touchend events.
                    ev.preventDefault();

                    element.ontouchmove = (ev) => {
                        ev.preventDefault();
                        this.workspace.getCanvas().dispatchEvent(new TouchEvent('touchmove', {
                            targetTouches: Array.from(ev.targetTouches)
                        }));
                    }
                    element.ontouchend = (ev) => {
                        element.ontouchmove = null;
                        element.ontouchend = null;

                        ev.preventDefault();
                        this.workspace.getCanvas().dispatchEvent(new TouchEvent('touchend', {
                            targetTouches: Array.from(ev.targetTouches)
                        }));
                    }
                }
            }
            catch (err) {
                console.error(err);
            }
        };
    }

    addBlock(block: FlowBlockOptions) {
        this.blocks.push(block);
    }
}
