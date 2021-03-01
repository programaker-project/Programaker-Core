import { BlockExhibitor, BlockGenerator } from './block_exhibitor';
import { BlockManager } from './block_manager';
import { FlowBlock, Position2D, FlowBlockOptions, FlowActuator } from './flow_block';
import { FlowWorkspace } from './flow_workspace';
import { UiSignalService } from 'app/services/ui-signal.service';
import { Session } from 'app/session';
import { ADVANCED_CATEGORY, INTERNAL_CATEGORY } from './base_toolbox_description';
import { uuidv4 } from './utils';

export type ActuatorGenerator = () => FlowActuator;

export class Toolbox {
    toolboxDiv: HTMLDivElement;
    blockShowcase: HTMLDivElement;
    categories: { [key: string]: HTMLDivElement } = {};
    categoryShortcuts: { [key: string]: HTMLLIElement } = {};
    blocks: FlowBlockOptions[] = [];
    categoryShortcutList: HTMLUListElement;

    public static BuildOn(baseElement: HTMLElement,
                          workspace: FlowWorkspace,
                          uiSignalService: UiSignalService,
                          session: Session,
                          logic_only: boolean,
                         ): Toolbox {
        let toolbox: Toolbox;
        try {
            toolbox = new Toolbox(baseElement, workspace, uiSignalService, session, logic_only);
            toolbox.init();
        }
        catch(err) {
            toolbox.dispose();

            throw err;
        }

        return toolbox;
    }


    private constructor(private baseElement: HTMLElement,
                        private workspace: FlowWorkspace,
                        public uiSignalService: UiSignalService,
                        private session: Session,
                        private logic_only: boolean,
                       ) { }

    onResize() {}

    dispose() {
        this.baseElement.removeChild(this.toolboxDiv);
    }

    init() {
        if (this.logic_only) {
            return;
        }

        this.toolboxDiv = document.createElement('div');
        this.toolboxDiv.setAttribute('class', 'toolbox');
        this.baseElement.appendChild(this.toolboxDiv);

        this.categoryShortcutList = document.createElement('ul');
        this.categoryShortcutList.setAttribute('class', 'category-shortcut-list');
        this.toolboxDiv.appendChild(this.categoryShortcutList);

        this.blockShowcase = document.createElement('div');
        this.blockShowcase.setAttribute('class', 'showcase');
        this.toolboxDiv.appendChild(this.blockShowcase);
    }

    setCategory(cat:{ id: string, name: string }) {
        const [div, updated] = this.getOrCreateCategory(cat);
        if (!updated && !this.logic_only) {
            const title = div.getElementsByClassName('category_title')[0] as HTMLDivElement;
            title.innerText = cat.name;
        }
    }

    private getOrCreateCategory(cat:{ id: string, name: string }): [HTMLDivElement, boolean, HTMLLIElement] {
        let category_div = this.categories[cat.id];
        let categoryShortcut = this.categoryShortcuts[cat.id];
        let created_now = false;

        if (!category_div && !this.logic_only) {
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

            categoryShortcut = this.categoryShortcuts[cat.id] = document.createElement('li');
            categoryShortcut.setAttribute('class', 'empty');

            const catName = document.createElement('div');
            catName.setAttribute('class', 'category-name');
            catName.innerText = cat.name;
            categoryShortcut.appendChild(catName);

            categoryShortcut.onclick = () => {
                // Expand if it's collapsed
                if (category_div.classList.contains('collapsed')) {
                    category_div.classList.remove('collapsed');
                }

                // Then scroll to it
                category_div.scrollIntoView({ behavior: "smooth", block: "start", inline: "nearest", });
            };
            this.categoryShortcutList.appendChild(categoryShortcut);
        }

        return [category_div, created_now, categoryShortcut];
    }

    addBlockGenerator(generator: BlockGenerator, category_id: string) {
        if (this.logic_only) {
            return;
        }

        if (category_id === ADVANCED_CATEGORY) {
            if (!this.session.tags.is_advanced) {
                return; // Skip advaced blocks if the user has not activated them
            }
        }

        if (category_id === INTERNAL_CATEGORY) {
            return; // Don't show internal blocks
        }

        const [category_div, _created_now, category_shortcut] = this.getOrCreateCategory({ id: category_id, name: category_id })
        category_div.classList.remove('empty');
        category_shortcut.classList.remove('empty');

        const block_exhibitor = BlockExhibitor.FromGenerator(generator, category_div);
        const element = block_exhibitor.getElement();
        element.onmousedown = element.ontouchstart = (ev: MouseEvent | TouchEvent) => {
            try {
                const rect = block_exhibitor.getInnerElementRect();

                if (!rect) {
                    // Hidden block, ignore
                    return;
                }

                const block = generator(this.workspace, uuidv4());
                element.classList.add('hidden');
                this.toolboxDiv.classList.add('subsumed');

                const pos = this.workspace._getPositionFromEvent(ev);
                // Center rect on cursor
                rect.x = pos.x - (rect.width / this.workspace.getInvZoomLevel()) / 2;
                rect.y = pos.y - (rect.height / this.workspace.getInvZoomLevel()) / 2;

                const block_id = this.workspace.drawAbsolute(block, rect);

                (this.workspace as any)._mouseDownOnBlock(pos, block, (ev: Position2D) => {

                    element.classList.remove('hidden');
                    this.toolboxDiv.classList.remove('subsumed');

                    // Check if the block was dropped on the toolbox, if so remove it
                    const toolboxRect = this.toolboxDiv.getBoundingClientRect();
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

    addActuator(generator: ActuatorGenerator, category_id: string) {
        if (this.logic_only) {
            return;
        }

        if (category_id === ADVANCED_CATEGORY) {
            if (!this.session.tags.is_advanced) {
                return; // Skip advaced blocks if the user has not activated them
            }
        }

        const [category_div, _created_now, category_shortcut] = this.getOrCreateCategory({ id: category_id, name: category_id })
        category_div.classList.remove('empty');
        category_shortcut.classList.remove('empty');

        const actuator = generator();
        const element = actuator.render(category_div);
        element.onclick = (ev: MouseEvent) => {
            actuator.onclick();
        };
    }

    addBlock(block: FlowBlockOptions) {
        this.blocks.push(block);
    }
}
