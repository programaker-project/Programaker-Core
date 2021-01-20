import { async, TestBed } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';
import { doesNotChangePositionsOnReposition } from './utils';
import { MIN_WIDTH as MIN_PAGE_WIDTH } from '../../../flow-editor/ui-blocks/renderers/responsive_page';

describe('FlowUI positioning: 05. Image positioning.', () => {

    beforeEach(async(() => {
        configureTestBed(TestBed);
    }));

    it('IMAGE positioning with other elements in OTHER section.', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, blocks, page] = pageGraph([
            {
                type: "horizontal_ui_section",
                x: 10, y: 100,
                contents: [
                    {
                        type: 'fixed_image',
                        x: 10, y: 10,
                        extra: { dimensions: { width: MIN_PAGE_WIDTH + 100, height: MIN_PAGE_WIDTH + 100 }},
                    },
                ],
                extra: { dimensions: { width: 500, height: 500 }},
            },
            {
                type: "horizontal_ui_section",
                x: 10, y: 101,
                contents: [
                    {
                        type: 'simple_button',
                        x: 10, y: 10,
                    },
                ],
                extra: { dimensions: { width: 500, height: 500 }},
            },
        ]);

        const [ top, top_img, bot, bot_btn ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(5);
        /**
         * Layout:
         *
         *  +------------+
         *  |            |
         *  | +-------+  |
         *  | | Image |  |
         *  | +-------+  |
         *  | Horizontal |
         *  |    Section |
         *  +------------+
         *  +------------+
         *  |            |
         *  | +--------+ |
         *  | | Button | |
         *  | +--------+ |
         *  | Horizontal |
         *  |    Section |
         *  +------------+
         *
         **/

        const check = () => {
            const pageArea = workspace.getBlock(page).getBodyArea();

            for (const [name, section, element] of [['top', top, top_img], ['bottom', bot, bot_btn]]) {
                // Element position on section
                const areaSection = workspace.getBlock(section).getBodyArea();
                const areaElement = workspace.getBlock(element).getBodyArea();

                expect(areaSection.height)
                    .toBe(areaElement.height + SEPARATION * 2,
                          `On section-group ${name}. Height=${areaSection.height} =/= ${areaElement.height} + ${SEPARATION} * 2 `);

                expect(areaSection.y + SEPARATION)
                    .toBe(areaElement.y,
                          `On section-group ${name}. Y=${areaSection.y} + ${SEPARATION} =/= ${areaElement.y}`);

                // Everything inside the page
                expect(areaSection.x)
                    .toBe(pageArea.x,
                          `On section-group ${name}. X=${areaSection.x} =/= ${pageArea.x}`);

                expect(areaElement.x)
                    .toBeGreaterThanOrEqual(pageArea.x,
                          `On section-group ${name}. X=${areaElement.x} < ${pageArea.x}`);
            }
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });
});
