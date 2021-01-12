import { async, TestBed } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';
import { doesNotChangePositionsOnReposition } from './utils';

fdescribe('FlowUI positioning: 04. Position sections in a single step. ', () => {

    beforeEach(async(() => {
        configureTestBed(TestBed);
    }));

    it('3 elements in nested section .', async () => {
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
                        dimensions: { width: 300, height: 300 },
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
                        dimensions: { width: 300, height: 300 },
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
                        dimensions: { width: 300, height: 300 },
                    }
                ]
            },
        ]);

        const [ section, left, left_btn, center, center_btn, right, right_btn ] = blocks;

        workspace.load(graph);
        workspace.repositionAll();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(8);
        /**
         * Layout:
         *
         *     +--------------------------------------+
         *     |+-----------++-----------++----------+|
         *     || +-------+ ||+---------+||+--------+||
         *     || | LeftB | ||| CenterB |||| RightB |||
         *     || +-------+ ||+---------+||+--------+||
         *     ||  Vert     || Vert      || Vert     ||
         *     ||  Section  ||  Section  ||  Section ||
         *     |+-----------++-----------++----------+|
         *     |           Horizontal Section         |
         *     +--------------------------------------+
         *
         **/

        const check = () => {
            // Button alignment
            expect(result.nodes[left_btn].position.y).toEqual(result.nodes[center_btn].position.y);
            expect(result.nodes[center_btn].position.y).toEqual(result.nodes[right_btn].position.y);

            expect(result.nodes[left_btn].position.x).toBeLessThan(result.nodes[center_btn].position.x);
            expect(result.nodes[center_btn].position.x).toBeLessThan(result.nodes[right_btn].position.x);

            // Section alignment
            expect(result.nodes[left].position.y).toEqual(result.nodes[center].position.y);
            expect(result.nodes[center].position.y).toEqual(result.nodes[right].position.y);

            expect(result.nodes[left].position.x).toBeLessThan(result.nodes[center].position.x);
            expect(result.nodes[center].position.x).toBeLessThan(result.nodes[right].position.x);


            // expect(result.nodes[left].position.y
            //         + workspace.getBlock(up).getBodyArea().height
            //         + SEPARATION).toEqual(result.nodes[down].position.y);
        };

        // Check once
        check();

        // Re-position to check for stability
        doesNotChangePositionsOnReposition(workspace, blocks);

        // Check again
        check();
        });

});
