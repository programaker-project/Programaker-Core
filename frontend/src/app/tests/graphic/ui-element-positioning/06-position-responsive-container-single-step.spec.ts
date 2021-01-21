import { async, TestBed } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';
import { doesNotChangePositionsOnReposition } from './utils';
import { MAX_WIDTH as TEXT_MAX_WIDTH } from 'app/flow-editor/ui-blocks/renderers/fixed_text';

describe('FlowUI positioning: 06. Position responsive container.', () => {

    beforeEach(async(() => {
        configureTestBed(TestBed);
    }));

    it('Simple UI cards should have a stable position', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, blocks, page] = pageGraph([
            {
                type: "simple_card",
                x: 10, y: 10,
                contents: [
                    {
                        type: 'horizontal_ui_section',
                        x: 10, y: 10,
                        contents: [
                            {
                                type: 'simple_button',
                                x: 10, y: 10,
                            },
                        ],
                    },
                ],
            },
        ]);

        const [ card, section, button ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(blocks.length + 1);
        /**
         * Layout:
         *    +--------------+
         *    |+------------+|
         *    || +--------+ ||
         *    || | Button | ||
         *    || +--------+ ||
         *    || Horizontal ||
         *    ||    Section ||
         *    |+------------+|
         *    |   UI Card    |
         *    +--------------+
         *
         **/


        const check = () => {
            const cardArea = workspace.getBlock(card).getBodyArea();
            const sectionArea = workspace.getBlock(section).getBodyArea();
            const buttonArea = workspace.getBlock(button).getBodyArea();
            const pageArea = workspace.getBlock(page).getBodyArea();

            // Card has to has to take all width (minus separation)
            expect(cardArea.x)
                .toBe(pageArea.x + SEPARATION,
                      `${cardArea.x} =/= ${pageArea.x} + ${SEPARATION}`);
            expect(cardArea.width + SEPARATION * 2)
                .toBe(pageArea.width,
                      `${cardArea.width} + ${SEPARATION} * 2 =/= ${pageArea.width}`);

            // Section has to take all width on card, and all height minus separation
            expect(sectionArea.x).toEqual(cardArea.x);
            expect(sectionArea.width).toEqual(cardArea.width);

            expect(sectionArea.y)
                .toBe(cardArea.y + SEPARATION,
                      `${sectionArea.y} =/= ${cardArea.y} + ${SEPARATION}`);
            expect(sectionArea.height + SEPARATION * 2)
                .toBe(cardArea.height,
                      `${sectionArea.height} + ${SEPARATION} * 2 =/= ${cardArea.height}`);

            // Button must be SEPARATION to the left & top of the section
            expect(sectionArea.y + SEPARATION)
                .toBe(buttonArea.y,
                      `${sectionArea.y} + ${SEPARATION} =/= ${buttonArea.y}`);

            expect(sectionArea.x + SEPARATION)
                .toBe(buttonArea.x,
                      `${sectionArea.x} + ${SEPARATION} =/= ${buttonArea.x}`);

            expect(buttonArea.y + buttonArea.height + SEPARATION)
                .toBe(sectionArea.y + sectionArea.height,
                      `${buttonArea.y} + ${buttonArea.height} + ${SEPARATION} =/= ${sectionArea.y} + ${sectionArea.height}`);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });

    it('Simple UI cards should have a stable position even out of Pages', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, blocks, _page] = pageGraph([
            {
                type: "simple_card",
                x: 10, y: 10,
                contents: [
                    {
                        type: 'horizontal_ui_section',
                        x: 10, y: 10,
                        contents: [
                            {
                                type: 'simple_button',
                                x: 10, y: 10,
                            },
                        ],
                    },
                ],
            },
        ], { noPage: true });

        const [ card, section, button ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(blocks.length);
        /**
         * Layout:
         *    +--------------+
         *    |+------------+|
         *    || +--------+ ||
         *    || | Button | ||
         *    || +--------+ ||
         *    || Horizontal ||
         *    ||    Section ||
         *    |+------------+|
         *    |   UI Card    |
         *    +--------------+
         *
         **/

        const check = () => {
            const cardArea = workspace.getBlock(card).getBodyArea();
            const sectionArea = workspace.getBlock(section).getBodyArea();
            const buttonArea = workspace.getBlock(button).getBodyArea();

            // Section has to take all width on card, and all height minus separation
            expect(sectionArea.x).toEqual(cardArea.x);
            expect(sectionArea.width).toEqual(cardArea.width);

            expect(sectionArea.y)
                .toBe(cardArea.y + SEPARATION,
                      `${sectionArea.y} =/= ${cardArea.y} + ${SEPARATION}`);
            expect(sectionArea.height + SEPARATION * 2)
                .toBe(cardArea.height,
                      `${sectionArea.height} + ${SEPARATION} * 2 =/= ${cardArea.height}`);

            // Button must be SEPARATION to the left & top of the section
            expect(sectionArea.y + SEPARATION)
                .toBe(buttonArea.y,
                      `${sectionArea.y} + ${SEPARATION} =/= ${buttonArea.y}`);

            expect(sectionArea.x + SEPARATION)
                .toBe(buttonArea.x,
                      `${sectionArea.x} + ${SEPARATION} =/= ${buttonArea.x}`);

            expect(buttonArea.y + buttonArea.height + SEPARATION)
                .toBe(sectionArea.y + sectionArea.height,
                      `${buttonArea.y} + ${buttonArea.height} + ${SEPARATION} =/= ${sectionArea.y} + ${sectionArea.height}`);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });
});
