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
    hideButtonDiv: HTMLDivElement;
    blockShowcase: HTMLDivElement;
    categories: { [key: string]: { div: HTMLDivElement, content: HTMLDivElement } } = {};
    categoryShortcuts: { [key: string]: HTMLLIElement } = {};
    blocks: FlowBlockOptions[] = [];
    categoryShortcutList: HTMLDivElement;
    categoryShortcutListContents: HTMLUListElement;

    public static BuildOn(baseElement: HTMLElement,
                          workspace: FlowWorkspace,
                          uiSignalService: UiSignalService,
                          session: Session,
                          no_dom: boolean,
                          behavior: { portrait: boolean, autohide: boolean },
                         ): Toolbox {
        let toolbox: Toolbox;
        try {
            toolbox = new Toolbox(baseElement, workspace, uiSignalService, session, no_dom, behavior);
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
                        private no_dom: boolean,
                        private behavior: { portrait: boolean, autohide: boolean },
                       ) { }

    onResize() {}

    dispose() {
        this.baseElement.removeChild(this.toolboxDiv);
    }

    init() {
        if (this.no_dom) {
            return;
        }

        // Toolbox
        this.toolboxDiv = document.createElement('div');
        const classes = this.toolboxDiv.classList;
        classes.add('toolbox');
        if (this.behavior.portrait) {
            classes.add('portrait')
        }
        else {
            classes.add('landscape');
        }
        if (this.behavior.autohide) {
            classes.add('collapsed');
        }

        // Hide button
        this.hideButtonDiv = document.createElement('div');
        const button = document.createElement('button');
        button.onclick = () => {
            classes.add('collapsed');
        }
        button.innerText = '⌄';
        this.hideButtonDiv.setAttribute('class', 'hide-button-section');
        this.hideButtonDiv.appendChild(button);
        this.toolboxDiv.appendChild(this.hideButtonDiv);

        this.baseElement.appendChild(this.toolboxDiv);

        this.categoryShortcutList = document.createElement('div');
        this.categoryShortcutList.setAttribute('class', 'category-shortcut-list');

        this.categoryShortcutListContents = document.createElement('ul');
        this.categoryShortcutListContents.setAttribute('class', 'contents');
        this.categoryShortcutList.appendChild(this.categoryShortcutListContents);
        this.toolboxDiv.appendChild(this.categoryShortcutList);

        this.blockShowcase = document.createElement('div');
        this.blockShowcase.setAttribute('class', 'showcase');
        this.toolboxDiv.appendChild(this.blockShowcase);
    }

    setCategory(cat:{ id: string, name: string }) {
        const [div, updated] = this.getOrCreateCategory(cat);
        if (!updated && !this.no_dom) {
            const title = div.getElementsByClassName('category_title')[0] as HTMLDivElement;
            title.innerText = cat.name;
        }
    }

    private getOrCreateCategory(cat:{ id: string, name: string }): [HTMLDivElement, HTMLDivElement, boolean, HTMLLIElement] {
        let category = this.categories[cat.id];
        let categoryShortcut = this.categoryShortcuts[cat.id];
        let created_now = false;
        let category_div: HTMLDivElement;
        let category_content: HTMLDivElement;

        if (!category && !this.no_dom) {
            // Category
            category_div = document.createElement('div');
            category_div.setAttribute('class', 'category empty cat_name_' + cat.name + ' cat_id_' + cat.id);
            this.blockShowcase.appendChild(category_div);

            // Contents
            category_content = document.createElement('div');
            category_content.setAttribute('class', 'content');
            category_div.appendChild(category_content);

            // Title
            const cat_title = document.createElement('div');
            cat_title.setAttribute('class', 'category_title');
            cat_title.innerText = cat.name;
            category_content.appendChild(cat_title)

            this.categories[cat.id] = {div: category_div, content: category_content};

            created_now = true;

            categoryShortcut = this.categoryShortcuts[cat.id] = document.createElement('li');
            categoryShortcut.setAttribute('class', 'empty');

            const catName = document.createElement('div');
            catName.setAttribute('class', 'category-name');
            catName.innerText = cat.name;
            categoryShortcut.appendChild(catName);

            categoryShortcut.onclick = () => {
                // Expand if it's collapsed
                if (this.toolboxDiv.classList.contains('collapsed')) {
                    this.toolboxDiv.classList.remove('collapsed');
                }

                // Then scroll to it
                category_div.scrollIntoView({ behavior: "smooth", block: "start", inline: "nearest", });
            };
            this.categoryShortcutListContents.appendChild(categoryShortcut);
        }
        else if (!this.no_dom) {
            category_div = category.div;
            category_content = category.content;
        }

        return [category_div, category_content, created_now, categoryShortcut];
    }

    addBlockGenerator(generator: BlockGenerator, category_id: string) {
        if (this.no_dom) {
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

        const [category_div, category_content, _created_now, category_shortcut] = this.getOrCreateCategory({ id: category_id, name: category_id })
        category_div.classList.remove('empty');
        category_content.classList.remove('empty');
        category_shortcut.classList.remove('empty');

        const block_exhibitor = BlockExhibitor.FromGenerator(generator, category_content);
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
        if (this.no_dom) {
            return;
        }

        if (category_id === ADVANCED_CATEGORY) {
            if (!this.session.tags.is_advanced) {
                return; // Skip advaced blocks if the user has not activated them
            }
        }

        const [category_div, category_content, _created_now, category_shortcut] = this.getOrCreateCategory({ id: category_id, name: category_id })
        category_div.classList.remove('empty');
        category_content.classList.remove('empty');
        category_shortcut.classList.remove('empty');

        const actuator = generator();
        const element = actuator.render(category_content);
        element.onclick = (ev: MouseEvent) => {
            actuator.onclick();
        };
    }

    addBlock(block: FlowBlockOptions) {
        this.blocks.push(block);
    }
}
