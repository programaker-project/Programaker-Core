import { TestBed, waitForAsync } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';

describe('FlowUI positioning: 03. Bad cases. ', () => {

    beforeEach(waitForAsync(() => {
        configureTestBed(TestBed);
    }));

    it('Element right of horizontal separator.', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, [ top, bot, sep, right ], page] = pageGraph([
            {
                type: 'simple_button',
                x: 10, y: 10,
            },
            {
                type: 'simple_button',
                x: 10, y: 200
            },
            {
                type: 'horizontal_separator',
                x: 2, y: 100,
            },
            {
                type: 'simple_button',
                x: 10, y: 100,
            },
        ]);

        workspace.load(graph);
        workspace.repositionAll();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(5);
        /**
         * Source:
         * +--------+
         * |  Btn1  |
         * +--------+
         * +-----------------+ +--------+
         * | Horiz separator | |  Btn3  |
         * +-----------------+ +--------+
         * +--------+
         * |  Btn2  |
         * +--------+
         *
         *
         * Expected result:
         * +--------+
         * |  Btn1  |
         * +--------+
         * +-----------------+
         * | Horiz separator |
         * +-----------------+
         * +--------+
         * |  Btn3  |
         * +--------+
         * +--------+
         * |  Btn2  |
         * +--------+
         */

        const check = () => {
            for (const [up, down] of [[ top, sep ], [sep, right], [right, bot]]) {
                expect(result.nodes[up].position.y
                    + workspace.getBlock(up).getBodyArea().height
                    + SEPARATION).toEqual(result.nodes[down].position.y);
            }
        };

        // Check once
        check();

        // Re-position
        workspace.repositionAll();

        // Check again
        check();
    });

});
