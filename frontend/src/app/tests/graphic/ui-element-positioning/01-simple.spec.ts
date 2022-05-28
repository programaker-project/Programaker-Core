import { TestBed, waitForAsync } from '@angular/core/testing';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowGraph } from '../../../flow-editor/flow_graph';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SEPARATION as PositioningSeparation } from 'app/flow-editor/ui-blocks/renderers/positioning';
import { configureTestBed } from './builder';

describe('FlowUI positioning: 01. Simple positioning. ', () => {

    beforeEach(waitForAsync(() => {
        configureTestBed(TestBed);
    }));

    it('Should work vertically', async () => {
        const fixture = TestBed.createComponent(FlowEditorComponent);

        const app: FlowEditorComponent = fixture.debugElement.componentInstance;
        expect(app).toBeInstanceOf(FlowEditorComponent);
        await app.ngOnInit();

        const workspace = app.workspace;
        expect(workspace).toBeInstanceOf(FlowWorkspace);

        const graph: FlowGraph = { edges: [], nodes: {
            // Page
            page: {
	            data: {
		            value: {
			            "options": {
				            "type": "ui_flow_block",
				            "subtype": "container_flow_block",
				            "outputs": [],
				            "isPage": true,
				            "inputs": [],
				            "id": "responsive_page_holder",
			            },
			            "extra": {
				            "dimensions": {
					            "width": 888.9020385742188,
					            "height": 856.9749755859375
				            }
			            }
		            },
		            type: "ui_flow_block",
		            subtype: "container_flow_block"
	            },
                container_id: null,
                position: { x: 0, y: 0 },
            },

            // Ui Horizontal Section
            container: {
	            data: {
		            "value": {
			            "options": {
				            "type": "ui_flow_block",
				            "subtype": "container_flow_block",
				            "outputs": [],
				            "inputs": [],
				            "id": "horizontal_ui_section",
			            },
			            "extra": {
				            "dimensions": {
					            "width": 886.9020385742188,
					            "height": 53.024993896484375
				            }
			            }
		            },
		            "type": "ui_flow_block",
		            "subtype": "container_flow_block"
                },
                container_id: 'page',
                position: { x: 10, y: 10 },
            },

            // Simple element
            el1: {
	            data: {
		            "value": {
			            "options": {
				            "type": "ui_flow_block",
				            "outputs": [
					            {
						            "type": "pulse"
					            },
					            {
						            "type": "string",
						            "name": "button text"
					            }
				            ],
				            "inputs": [],
				            "id": "simple_button",
			            },
			            "extra": {
				            "textContent": "Element 1"
			            }
		            },
		            "type": "ui_flow_block"
	            },
                container_id: 'container',
                position: { x: 5, y: 20 },
            },

            // Simple element
            el2: {
	            data: {
		            "value": {
			            "options": {
				            "type": "ui_flow_block",
				            "outputs": [
					            {
						            "type": "pulse"
					            },
					            {
						            "type": "string",
						            "name": "button text"
					            }
				            ],
				            "inputs": [],
				            "id": "simple_button",
			            },
			            "extra": {
				            "textContent": "Element 2"
			            }
		            },
		            "type": "ui_flow_block"
	            },
                container_id: 'container',
                position: { x: 7, y: 20 },
            },
        }};
        workspace.load(graph);

        workspace.repositionAll();

        const result = workspace.getGraph();

        expect(Object.keys(result.nodes).length).toBe(4);

        // Sample assertions
        expect(result.nodes.el1.position.x).toBeGreaterThan(result.nodes.container.position.x);
        expect(result.nodes.el2.position.x).toBeGreaterThan(result.nodes.container.position.x);

        expect(result.nodes.el2.position.x).toBeGreaterThan(result.nodes.el1.position.x + PositioningSeparation);

        // Might be greater than because of the borders
        expect(result.nodes.container.position.x).toBeGreaterThanOrEqual(result.nodes.page.position.x);
    });
});
