import { async, TestBed } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';
import { doesNotChangePositionsOnReposition } from './utils';

describe('FlowUI positioning: 07. Position responsive container inside section.', () => {

    beforeEach(async(() => {
        configureTestBed(TestBed);
    }));

    // ----------------------------
    // UI Card tests
    // ----------------------------
    it('Simple UI cards should have a stable position', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, blocks, page] = pageGraph([
            {
                type: 'horizontal_ui_section',
                x: 10, y: 10,
                contents: [
                    {
                        type: "simple_button",
                        x: 10, y: 10,
                    },
                    {
                        type: "simple_card",
                        x: 11, y: 10,
                        contents: [],
                        extra: { dimensions: { width: 200, height: 200, } }
                    },
                    {
                        type: "simple_button",
                        x: 12, y: 10,
                    },
                ],
            },
        ]);

        const [ section, leftButton, card, rightButton ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(blocks.length + 1);
        /**
         * Layout:
         *
         * +------------------------------------------------+
         * | +-------------+ +-----------+ +--------------+ |
         * | | Left button | |  Ui Card  | | Right button | |
         * | +-------------+ +-----------+ +--------------+ |
         * |              Horizontal Section                |
         * +------------------------------------------------+
         *
         *
         **/

        const check = () => {
            const pageArea = workspace.getBlock(page).getBodyArea();
            const sectionArea = workspace.getBlock(section).getBodyArea();
            const leftButtonArea = workspace.getBlock(leftButton).getBodyArea();
            const cardArea = workspace.getBlock(card).getBodyArea();
            const rightButtonArea = workspace.getBlock(rightButton).getBodyArea();

            // Section has to take all width on page, and all height minus separation
            expect(sectionArea.x).toEqual(pageArea.x);
            expect(sectionArea.width).toEqual(pageArea.width);

            for (const [name, elem] of [['left button', leftButton], ['center card', card], ['right button', rightButton]]) {
                const eArea = workspace.getBlock(elem).getBodyArea();

                // Height
                expect(eArea.y)
                    .toBeGreaterThanOrEqual(sectionArea.y + SEPARATION,
                                            `Element: ${name}. ${eArea.y} < ${sectionArea.y} + ${SEPARATION}`);

                expect(eArea.y + eArea.height + SEPARATION)
                    .toBeLessThanOrEqual(sectionArea.y + sectionArea.height,
                                            `Element: ${name}. ${eArea.y} + ${eArea.height} + ${SEPARATION} > ${sectionArea.y} + ${sectionArea.height}`);
            }

            for (const [name, left, right ] of [['left -> card', leftButton, card], ['card -> right', card, rightButton]]) {
                const leftArea = workspace.getBlock(left).getBodyArea();
                const rightArea = workspace.getBlock(right).getBodyArea();

                // Relative positioning
                expect(leftArea.x + leftArea.width + SEPARATION)
                    .toBe(rightArea.x,
                          `Group ${name}. ${leftArea.x} + ${leftArea.width} + ${SEPARATION} =/= ${rightArea.x}`);


            }

            expect(leftButtonArea.x)
                .toBe(sectionArea.x + SEPARATION,
                      `Left button X. ${leftButtonArea.x} =/= ${sectionArea.x} + ${SEPARATION}`);

            expect(rightButtonArea.x + rightButtonArea.width + SEPARATION)
                .toBe(sectionArea.x + sectionArea.width,
                      `Right button X. ${rightButtonArea.x} + ${rightButtonArea.width} + ${SEPARATION} =/= ${sectionArea.x} + ${sectionArea.width}`);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });

    // ----------------------------
    // Link Area tests
    // ----------------------------
    it('LinkAreas should have a stable position', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, blocks, page] = pageGraph([
            {
                type: 'horizontal_ui_section',
                x: 10, y: 10,
                contents: [
                    {
                        type: "simple_button",
                        x: 10, y: 10,
                    },
                    {
                        type: "link_area",
                        x: 11, y: 10,
                        contents: [],
                        extra: { dimensions: { width: 200, height: 200, } }
                    },
                    {
                        type: "simple_button",
                        x: 12, y: 10,
                    },
                ],
            },
        ]);

        const [ section, leftButton, link, rightButton ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(blocks.length + 1);
        /**
         * Layout:
         *
         * +-------------------------------------------------+
         * | +-------------+ +------------+ +--------------+ |
         * | | Left button | |  Link Area | | Right button | |
         * | +-------------+ +------------+ +--------------+ |
         * |              Horizontal Section                 |
         * +-------------------------------------------------+
         *
         *
         **/

        const check = () => {
            const pageArea = workspace.getBlock(page).getBodyArea();
            const sectionArea = workspace.getBlock(section).getBodyArea();
            const leftButtonArea = workspace.getBlock(leftButton).getBodyArea();
            const linkArea = workspace.getBlock(link).getBodyArea();
            const rightButtonArea = workspace.getBlock(rightButton).getBodyArea();

            // Section has to take all width on page, and all height minus separation
            expect(sectionArea.x).toEqual(pageArea.x);
            expect(sectionArea.width).toEqual(pageArea.width);

            for (const [name, elem] of [['left button', leftButton], ['center linkArea', link], ['right button', rightButton]]) {
                const eArea = workspace.getBlock(elem).getBodyArea();

                // Height
                expect(eArea.y)
                    .toBeGreaterThanOrEqual(sectionArea.y + SEPARATION,
                                            `Element: ${name}. ${eArea.y} < ${sectionArea.y} + ${SEPARATION}`);

                expect(eArea.y + eArea.height + SEPARATION)
                    .toBeLessThanOrEqual(sectionArea.y + sectionArea.height,
                                            `Element: ${name}. ${eArea.y} + ${eArea.height} + ${SEPARATION} > ${sectionArea.y} + ${sectionArea.height}`);
            }

            for (const [name, left, right ] of [['left -> linkArea', leftButton, link], ['linkArea -> right', link, rightButton]]) {
                const leftArea = workspace.getBlock(left).getBodyArea();
                const rightArea = workspace.getBlock(right).getBodyArea();

                // Relative positioning
                expect(leftArea.x + leftArea.width + SEPARATION)
                    .toBe(rightArea.x,
                          `Group ${name}. ${leftArea.x} + ${leftArea.width} + ${SEPARATION} =/= ${rightArea.x}`);


            }

            expect(leftButtonArea.x)
                .toBe(sectionArea.x + SEPARATION,
                      `Left button X. ${leftButtonArea.x} =/= ${sectionArea.x} + ${SEPARATION}`);

            expect(rightButtonArea.x + rightButtonArea.width + SEPARATION)
                .toBe(sectionArea.x + sectionArea.width,
                      `Right button X. ${rightButtonArea.x} + ${rightButtonArea.width} + ${SEPARATION} =/= ${sectionArea.x} + ${sectionArea.width}`);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });
});
