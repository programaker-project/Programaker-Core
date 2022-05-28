import { TestBed, waitForAsync } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';
import { doesNotChangePositionsOnReposition } from './utils';
import { MAX_WIDTH as TEXT_MAX_WIDTH } from 'app/flow-editor/ui-blocks/renderers/fixed_text';

describe('FlowUI positioning: 04. Position sections in a single step.', () => {

    beforeEach(waitForAsync(() => {
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
                extra: { dimensions: { width: 9999, height: 9999 }},
            },
        ]);

        const [ topLevel, left_btn, center_btn, right_btn ] = blocks;

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

            const areaTopLevel = workspace.getBlock(topLevel).getBodyArea();
            // Button position on section
            for (const [name, btn] of [['left', left_btn], ['center', center_btn], ['right', right_btn]]) {
                const areaButton = workspace.getBlock(btn).getBodyArea();

                expect(areaTopLevel.height)
                    .toBe(areaButton.height + SEPARATION * 2,
                          `On section-group ${name}. Height=${areaTopLevel.height} =/= ${areaButton.height} + ${SEPARATION} * 2 `);

                expect(areaTopLevel.y + SEPARATION)
                    .toBe(areaButton.y,
                          `On section-group ${name}. Y=${areaTopLevel.y} + ${SEPARATION} =/= ${areaButton.y}`);
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
                        extra: { dimensions: { width: 9999, height: 9999 }},
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
                        extra: { dimensions: { width: 9999, height: 9999 }},
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
                        extra: { dimensions: { width: 9999, height: 9999 }},
                    }
                ],
                extra: { dimensions: { width: 9999, height: 9999 }},
            },
        ]);

        const [ topLevel, left, left_btn, center, center_btn, right, right_btn ] = blocks;

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

    it('3 elements in a SINGLE NESTED section in heavily distorted positions.', async () => {
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
                            {
                                type: 'simple_button',
                                x: 10, y: 11,
                            },
                            {
                                type: 'simple_button',
                                x: 10, y: 12,
                            },
                        ],
                        extra: { dimensions: { width: 9999, height: 9999 }},
                    }
                ],
                extra: { dimensions: { width: 9999, height: 9999 }},
            },
        ]);

        const [ topLevel, vert, top_btn, center_btn, bot_btn ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(6);
        /**
         * Layout:
         *
         *  +-------------+
         *  |+-----------+|
         *  ||+---------+||
         *  ||| Top Btn |||
         *  ||+---------+||
         *  ||+---------+||
         *  ||| CenterB |||
         *  ||+---------+||
         *  ||+---------+||
         *  ||| BottomB |||
         *  ||+---------+||
         *  || Vertical  ||
         *  ||   Section ||
         *  |+-----------+|
         *  | Horizontal  |
         *  |     Section |
         *  +-------------+
         *
         **/

        const check = () => {
            // Button alignment
            for (const [pair, pair_top, pair_bottom] of [['top-center', top_btn, center_btn], ['center-bot', center_btn, bot_btn]]) {
                const areaTop = workspace.getBlock(pair_top).getBodyArea();
                const areaBottom = workspace.getBlock(pair_bottom).getBodyArea();

                expect(areaTop.x).toBe(areaBottom.x);

                expect(areaTop.y + areaTop.height + SEPARATION)
                    .toBe(areaBottom.y,
                          `On button pair ${pair}. ${areaTop.y} + ${areaTop.height} + ${SEPARATION} =/= ${areaBottom.y}`);
            }

            const areaTopLevel = workspace.getBlock(topLevel).getBodyArea();
            // Button position on section
            for (const [name, btn] of [['top', top_btn], ['center', center_btn], ['bottom', bot_btn]]) {
                const areaButton = workspace.getBlock(btn).getBodyArea();

                expect(areaTopLevel.width)
                    .toBe(areaButton.width + SEPARATION * 4,
                          `On section-group ${name}. Width=${areaTopLevel.width} =/= ${areaButton.width} + ${SEPARATION} * 4`);

                expect(areaTopLevel.x + SEPARATION * 2)
                    .toBe(areaButton.x,
                          `On section-group ${name}. X=${areaTopLevel.x} + ${SEPARATION} * 2 =/= ${areaButton.x}`);
            }

            const areaTopButton = workspace.getBlock(top_btn).getBodyArea();
            const areaBotButton = workspace.getBlock(bot_btn).getBodyArea();

            expect(areaTopLevel.y + SEPARATION)
                .toBe(areaTopButton.y,
                      `Y=${areaTopLevel.y} + ${SEPARATION} =/= ${areaTopButton.y}`);

            expect(areaTopLevel.y + areaTopLevel.height)
                .toBe(areaBotButton.y + areaBotButton.height + SEPARATION,
                      `Y=${areaTopLevel.y} + ${areaTopLevel.height} =/= ${areaBotButton.y} + ${areaBotButton.height} + ${SEPARATION}`);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });

    it('3 elements in NESTED HORIZONTALS in a VERTICAL SINGLE NESTED section in heavily distorted positions.', async () => {
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
                                type: "horizontal_ui_section",
                                x: 10, y: 100,
                                contents: [
                                    {
                                        type: 'simple_button',
                                        x: 10, y: 10,
                                    },
                                ]
                            },
                            {
                                type: "horizontal_ui_section",
                                x: 10, y: 101,
                                contents: [
                                    {
                                        type: 'simple_button',
                                        x: 10, y: 10,
                                    },
                                ]
                            },
                            {
                                type: "horizontal_ui_section",
                                x: 10, y: 102,
                                contents: [
                                    {
                                        type: 'simple_button',
                                        x: 10, y: 10,
                                    },
                                ]
                            },
                        ],
                        extra: { dimensions: { width: 9999, height: 9999 }},
                    }
                ],
                extra: { dimensions: { width: 9999, height: 9999 }},
            },
        ]);

        const [ topLevel, vert, top, top_btn, center, center_btn, bot, bot_btn ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(9);
        /**
         * Layout:
         *
         *  +--------------------+
         *  |                    |
         *  | +---------------+  |
         *  | | +-----------+ |  |
         *  | | |+---------+| |  |
         *  | | || Top Btn || |  |
         *  | | |+---------+| |  |
         *  | | |Horizontal | |  |
         *  | | |   Section | |  |
         *  | | +-----------+ |  |
         *  | |               |  |
         *  | | +-----------+ |  |
         *  | | |+---------+| |  |
         *  | | || CenterB || |  |
         *  | | |+---------+| |  |
         *  | | |Horizontal | |  |
         *  | | |   Section | |  |
         *  | | +-----------+ |  |
         *  | |               |  |
         *  | | +-----------+ |  |
         *  | | |+---------+| |  |
         *  | | || BottomB || |  |
         *  | | |+---------+| |  |
         *  | | |Horizontal | |  |
         *  | | |   Section | |  |
         *  | | +-----------+ |  |
         *  | |  Vertical     |  |
         *  | |      Section  |  |
         *  | +---------------+  |
         *  |                    |
         *  |  Horizontal        |
         *  |      Section       |
         *  +--------------------+
         **/

        const check = () => {
            // Button alignment
            for (const [pair, pair_top, pair_bottom] of [['top-center', top_btn, center_btn], ['center-bot', center_btn, bot_btn]]) {
                const areaTop = workspace.getBlock(pair_top).getBodyArea();
                const areaBottom = workspace.getBlock(pair_bottom).getBodyArea();

                expect(areaTop.x).toBe(areaBottom.x);

                expect(areaTop.y + areaTop.height + SEPARATION * 2)
                    .toBe(areaBottom.y - SEPARATION,
                          `On button pair ${pair}. ${areaTop.y} + ${areaTop.height} + ${SEPARATION} =/= ${areaBottom.y} - ${SEPARATION}`);
            }

            // Section alignment
            for (const [pair, pair_top, pair_bottom] of [['top-center', top, center], ['center-bottom', center, bot]]) {
                const areaTop = workspace.getBlock(pair_top).getBodyArea();
                const areaBottom = workspace.getBlock(pair_bottom).getBodyArea();

                expect(areaTop.x).toBe(areaBottom.x);
                expect(areaTop.y + areaTop.height)
                    .toBe(areaBottom.y - SEPARATION,
                          `On section pair ${pair}. ${areaTop.y} + ${areaTop.height} =/= ${areaBottom.y} - ${SEPARATION}`);
            }

            // Button position on section
            for (const [name, section, btn] of [['top', top, top_btn], ['center', center, center_btn], ['bottom', bot, bot_btn]]) {
                const areaSection = workspace.getBlock(section).getBodyArea();
                const areaButton = workspace.getBlock(btn).getBodyArea();

                expect(areaSection.width)
                    .toBe(areaButton.width + SEPARATION * 2,
                          `On section-group ${name}. Width=${areaSection.width} =/= ${areaButton.width} + ${SEPARATION} * 2`);

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

            const areaTopLevel = workspace.getBlock(topLevel).getBodyArea();
            const areaTopButton = workspace.getBlock(top_btn).getBodyArea();
            const areaBotButton = workspace.getBlock(bot_btn).getBodyArea();

            expect(areaTopLevel.y + SEPARATION * 2)
                .toBe(areaTopButton.y,
                      `On section-group ${name}. Y=${areaTopLevel.y} + ${SEPARATION} * 2 =/= ${areaTopButton.y}`);

            expect(areaTopLevel.y + areaTopLevel.height)
                .toBe(areaBotButton.y + areaBotButton.height + SEPARATION * 2,
                      `Y=${areaTopLevel.y} + ${areaTopLevel.height} =/= ${areaBotButton.y} + ${areaBotButton.height} + ${SEPARATION} * 2`);

        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });


    it('Long and short FixedText separated by orizontalSeparator.', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, blocks, page] = pageGraph([
            {
                type: 'fixed_text',
                x: 10, y: 10,
                extra: {
                    content: [ {
                        value: 'A small text',
                        type: 'text'
                    }],
                },
            },
            {
                type: 'horizontal_separator',
                x: 10, y: 12,
                extra: {
                    dimensions: { width: TEXT_MAX_WIDTH * 2, height: 100 },
                }
            },
            {
                type: 'fixed_text',
                x: 10, y: 14,
                extra: {
                    content: [ {
                        value: 'A long text, which will take more than MAX_WIDTH and will require looking for the proper text break point so it fits on the allocated size, even if it is in a single line.',
                        type: 'text'
                    }],
                },
            },
        ]);

        const [ textTop, separator, textBot ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();
        workspace.center();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(blocks.length + 1);

        /**
         * Layout:
         *
         *       +---------+
         *       |  Text1  |
         *       +---------+
         *    +----------------+
         *    |   Separator1   |
         *    +----------------+
         *      +------------+
         *      | Long Text2 |
         *      +------------+
         *
         **/

        const check = () => {
            const topTextArea = workspace.getBlock(textTop).getBodyArea();
            const bottomTextArea = workspace.getBlock(textBot).getBodyArea();
            const separatorArea = workspace.getBlock(separator).getBodyArea();
            const pageArea = workspace.getBlock(page).getBodyArea();

            // Separator has to take all width
            expect(separatorArea.x).toEqual(pageArea.x);
            expect(separatorArea.width).toEqual(pageArea.width);

            // Top text (small) has to be centered
            expect(topTextArea.x + topTextArea.width / 2 )
                .toBe(pageArea.x + pageArea.width / 2,
                      `TopText centered in page. ${topTextArea.x} + ${topTextArea.width} / 2 =/= ${pageArea.x} + ${pageArea.width} / 2`);

            // Bottom text (large) has to take all width (minus separation)
            expect(bottomTextArea.x)
                .toBe(pageArea.x + SEPARATION,
                      `Bottom text must start just after SEPARATION. ${bottomTextArea.x} =/= ${pageArea.x} + ${SEPARATION}`);
            expect(bottomTextArea.width + SEPARATION * 2)
                .toBe(pageArea.width,
                     `Bottom text must take all width minus the SEPARATION left and right. ${bottomTextArea.width} + ${SEPARATION} * 2 =/= ${pageArea.width}`);


            // Vertically, there must be a separation between the elements
            expect(topTextArea.y + topTextArea.height + SEPARATION)
                .toBe(separatorArea.y,
                      `TopText → Separator. ${topTextArea.y} + ${topTextArea.height} + ${SEPARATION} =/= ${separatorArea.y}`);

            expect(separatorArea.y + separatorArea.height + SEPARATION)
                .toBe(bottomTextArea.y,
                      `Separator → BottomText. ${separatorArea.y} + ${separatorArea.height} + ${SEPARATION} =/= ${bottomTextArea.y}`);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
    });
});
