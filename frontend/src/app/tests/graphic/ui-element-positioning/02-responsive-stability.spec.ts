import { async, TestBed } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed, pageGraph } from './builder';

describe('FlowUI positioning: 02. Responsive stability. ', () => {

    beforeEach(async(() => {
        configureTestBed(TestBed);
    }));

    it('Should work in the first invocation and be stable.', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const [graph, [ topLeft, botLeft, right ]] = pageGraph([
            {
                type: 'simple_button',
                x: 10, y: 10,
            },
            {
                type: 'simple_button',
                x: 10, y: 20
            },
            {
                type: 'simple_button',
                x: 100, y: 10,
            }
        ]);

        workspace.load(graph);
        workspace.repositionAll();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(4);
        /**
         * Source:
         * +--------+       +--------+
         * |  Btn1  |       |  Btn3  |
         * +--------+       +--------+
         * +--------+
         * |  Btn2  |
         * +--------+
         *
         *
         * Expected result:
         * +--------+
         * |  Btn1  |
         * +--------+  +--------+
         *             |  Btn3  |
         * +--------+  +--------+
         * |  Btn2  |
         * +--------+
         */

        const check = () => {
            expect(result.nodes[topLeft].position.x
                + workspace.getBlock(topLeft).getBodyArea().width
                + SEPARATION).toEqual(result.nodes[right].position.x);
            expect(result.nodes[botLeft].position.x
                + workspace.getBlock(topLeft).getBodyArea().width
                + SEPARATION).toEqual(result.nodes[right].position.x);

            expect(result.nodes[topLeft].position.y
                + workspace.getBlock(topLeft).getBodyArea().height
                + SEPARATION).toEqual(result.nodes[botLeft].position.y);

            expect(result.nodes[topLeft].position.y).toBeLessThan(result.nodes[right].position.y);
            expect(result.nodes[botLeft].position.y).toBeGreaterThan(result.nodes[right].position.y);
        };

        // Check once
        check();

        // Re-position
        workspace.repositionAll();

        // Check again
        check();
    });

});
