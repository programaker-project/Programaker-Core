import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { async, TestBed } from '@angular/core/testing';
import { MatDialogModule } from '@angular/material/dialog';
import { MatMenuModule } from '@angular/material/menu';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { ConnectionService } from 'app/connection.service';
import { ProgramService } from 'app/program.service';
import { UiSignalService } from 'app/services/ui-signal.service';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { FlowGraph } from '../../../flow-editor/flow_graph';
import { FlowWorkspace } from '../../../flow-editor/flow_workspace';
import { SessionService } from '../../../session.service';
import { FakeConnectionService } from './fake-connection.service';
import { FakeCustomBlockService } from './fake-custom-block.service';
import { FakeProgramService } from './fake-program.service';
import { FakeServiceService } from './fake-service.service';
import { FakeSessionService } from './fake-session.service';
import { FakeUiSignalService } from './fake-ui-signal.service';
import { ServiceService } from 'app/service.service';
import { CustomBlockService } from 'app/custom_block.service';
import { SEPARATION as PositioningSeparation } from 'app/flow-editor/ui-blocks/renderers/positioning';

describe('FlowUI positioning: 01. Flow Editor positioning. ', () => {

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                BrowserCookiesModule.forRoot(),
                RouterTestingModule,
                HttpClientTestingModule,
                MatDialogModule,
                MatSnackBarModule,
                MatMenuModule,
                NoopAnimationsModule,
            ],
            declarations: [
                FlowEditorComponent,
            ],
            providers: [
                {
                    provide: CookiesService,
                    useClass: BrowserCookiesService,
                },
                {
                    provide: HttpClient,
                    useValue: {}
                },
                {
                    provide: SessionService,
                    useClass: FakeSessionService,
                },
                {
                    provide: ProgramService,
                    useClass: FakeProgramService,
                },
                {
                    provide: UiSignalService,
                    useClass: FakeUiSignalService,
                },
                {
                    provide: ConnectionService,
                    useClass: FakeConnectionService,
                },
                {
                    provide: ServiceService,
                    useClass: FakeServiceService,
                },
                {
                    provide: CustomBlockService,
                    useClass: FakeCustomBlockService,
                }
            ]
        });
    }));

    it('Should create the component', async () => {
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
