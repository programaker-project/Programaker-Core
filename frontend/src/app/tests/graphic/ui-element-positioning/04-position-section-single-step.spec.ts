import { async, TestBed } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';
import { doesNotChangePositionsOnReposition } from './utils';

describe('FlowUI positioning: 04. Position sections in a single step.', () => {

    beforeEach(async(() => {
        configureTestBed(TestBed);
    }));

    it('3 elements in NON-nested section in heavily distorted positions.', async () => {
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
                        type: 'simple_button',
                        x: 10, y: 10,
                    },
                    {
                        type: 'simple_button',
                        x: 10, y: 10,
                    },
                    {
                        type: 'simple_button',
                        x: 10, y: 10,
                    },
                ],
                dimensions: { width: 9999, height: 9999 },
            },
        ]);

        const [ section, left_btn, center_btn, right_btn ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(5);
        /**
         * Layout:
         *
         *     +------------------------------------+
         *     | +-------+  +---------+  +--------+ |
         *     | | LeftB |  | CenterB |  | RightB | |
         *     | +-------+  +---------+  +--------+ |
         *     |         Horizontal Section         |
         *     +------------------------------------+
         *
         **/

        const check = () => {
            // Button alignment
            for (const [pair, pair_left, pair_right] of [['left-center', left_btn, center_btn], ['center-right', center_btn, right_btn]]) {
                const areaLeft = workspace.getBlock(pair_left).getBodyArea();
                const areaRight = workspace.getBlock(pair_right).getBodyArea();

                expect(areaLeft.y).toBe(areaRight.y);

                expect(areaLeft.x + areaLeft.width)
                    .toBe(areaRight.x - SEPARATION,
                          `On button pair ${pair}. ${areaLeft.x} + ${areaLeft.width} =/= ${areaRight.x} - ${SEPARATION}`);
            }

            // Button position on section
            for (const [name, btn] of [['left', left_btn], ['center', center_btn], ['right', right_btn]]) {
                const areaSection = workspace.getBlock(section).getBodyArea();
                const areaButton = workspace.getBlock(btn).getBodyArea();

                expect(areaSection.height)
                    .toBe(areaButton.height + SEPARATION * 2,
                          `On section-group ${name}. Height=${areaSection.height} =/= ${areaButton.height} + ${SEPARATION} * 2 `);

                expect(areaSection.y + SEPARATION)
                    .toBe(areaButton.y,
                          `On section-group ${name}. Y=${areaSection.y} + ${SEPARATION} =/= ${areaButton.y}`);
            }
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });

    it('3 elements in NESTED section in heavily distorted positions.', async () => {
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
                        type: "horizontal_ui_section",
                        x: 10, y: 100,
                        contents: [
                            {
                                type: 'simple_button',
                                x: 10, y: 10,
                            },
                        ],
                        dimensions: { width: 9999, height: 9999 },
                    },
                    {
                        type: "horizontal_ui_section",
                        x: 10, y: 100,
                        contents: [
                            {
                                type: 'simple_button',
                                x: 10, y: 10,
                            },
                        ],
                        dimensions: { width: 9999, height: 9999 },
                    },
                    {
                        type: "horizontal_ui_section",
                        x: 10, y: 100,
                        contents: [
                            {
                                type: 'simple_button',
                                x: 10, y: 10,
                            },
                        ],
                        dimensions: { width: 9999, height: 9999 },
                    }
                ],
                dimensions: { width: 9999, height: 9999 },
            },
        ]);

        const [ section, left, left_btn, center, center_btn, right, right_btn ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(8);
        /**
         * Layout:
         *
         *     +------------------------------------+
         *     |+---------++-----------++----------+|
         *     ||+-------+||+---------+||+--------+||
         *     ||| LeftB |||| CenterB |||| RightB |||
         *     ||+-------+||+---------+||+--------+||
         *     || Vert    || Vert      || Vert     ||
         *     || Section ||  Section  ||  Section ||
         *     |+---------++-----------++----------+|
         *     |         Horizontal Section         |
         *     +------------------------------------+
         *
         **/

        const check = () => {
            // Button alignment
            for (const [pair, pair_left, pair_right] of [['left-center', left_btn, center_btn], ['center-right', center_btn, right_btn]]) {
                const areaLeft = workspace.getBlock(pair_left).getBodyArea();
                const areaRight = workspace.getBlock(pair_right).getBodyArea();

                expect(areaLeft.y).toBe(areaRight.y);

                expect(areaLeft.x + areaLeft.width + SEPARATION * 2)
                    .toBe(areaRight.x - SEPARATION,
                          `On button pair ${pair}. ${areaLeft.x} + ${areaLeft.width} + ${SEPARATION * 2} =/= ${areaRight.x} - ${SEPARATION}`);
            }

            // Section alignment
            for (const [pair, pair_left, pair_right] of [['left-center', left, center], ['center-right', center, right]]) {
                const areaLeft = workspace.getBlock(pair_left).getBodyArea();
                const areaRight = workspace.getBlock(pair_right).getBodyArea();

                expect(areaLeft.y).toBe(areaRight.y);
                expect(areaLeft.x + areaLeft.width)
                    .toBe(areaRight.x - SEPARATION,
                          `On section pair ${pair}. ${areaLeft.x} + ${areaLeft.width} =/= ${areaRight.x} - ${SEPARATION}`);
            }

            // Button position on section
            for (const [name, section, btn] of [['left', left, left_btn], ['center', center, center_btn], ['right', right, right_btn]]) {
                const areaSection = workspace.getBlock(section).getBodyArea();
                const areaButton = workspace.getBlock(btn).getBodyArea();

                expect(areaSection.width)
                    .toBe(areaButton.width + SEPARATION * 2,
                          `On section-group ${name}. Width=${areaSection.width} =/= ${areaButton.width} + ${SEPARATION} * 2 `);

                expect(areaSection.x + SEPARATION)
                    .toBe(areaButton.x,
                          `On section-group ${name}. X=${areaSection.x} + ${SEPARATION} =/= ${areaButton.x}`);

                expect(areaSection.height)
                    .toBe(areaButton.height + SEPARATION * 2,
                          `On section-group ${name}. Height=${areaSection.height} =/= ${areaButton.height} + ${SEPARATION} * 2 `);

                expect(areaSection.y + SEPARATION)
                    .toBe(areaButton.y,
                          `On section-group ${name}. Y=${areaSection.y} + ${SEPARATION} =/= ${areaButton.y}`);
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
