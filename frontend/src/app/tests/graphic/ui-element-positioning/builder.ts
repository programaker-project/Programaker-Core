import { HttpClient } from '@angular/common/http';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBedStatic } from '@angular/core/testing';
import { MatDialogModule } from '@angular/material/dialog';
import { MatMenuModule } from '@angular/material/menu';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { NoopAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';
import { CookiesService } from '@ngx-utils/cookies';
import { BrowserCookiesModule, BrowserCookiesService } from '@ngx-utils/cookies/browser';
import { ConnectionService } from 'app/connection.service';
import { CustomBlockService } from 'app/custom_block.service';
import { ProgramService } from 'app/program.service';
import { ServiceService } from 'app/service.service';
import { UiSignalService } from 'app/services/ui-signal.service';
import { FlowEditorComponent } from '../../../flow-editor/flow-editor.component';
import { SessionService } from '../../../session.service';
import { FakeConnectionService } from './fake-connection.service';
import { FakeCustomBlockService } from './fake-custom-block.service';
import { FakeProgramService } from './fake-program.service';
import { FakeServiceService } from './fake-service.service';
import { FakeSessionService } from './fake-session.service';
import { FakeUiSignalService } from './fake-ui-signal.service';
import { FlowGraph } from '../../../flow-editor/flow_graph';
import { uuidv4 } from '../../../flow-editor/utils';

export function configureTestBed(testBed: TestBedStatic) {
    testBed.configureTestingModule({
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
}

export function pageGraph(elements: { type: string, x: number, y: number }[]): [FlowGraph, string[]] {
    const graph: FlowGraph = { edges: [], nodes: {} };

    const pageId = uuidv4();
    graph.nodes[pageId] = {
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
                extra: {}
		    },
		    type: "ui_flow_block",
		    subtype: "container_flow_block"
	    },
        container_id: null,
        position: { x: 0, y: 0 },
    };

    const ids = [];

    for (const el of elements) {
        const id = uuidv4();
        ids.push(id);

        graph.nodes[id] = {
	        data: {
		        "value": {
			        "options": {
				        "type": "ui_flow_block",
				        "id": el.type,
			        },
                    extra: {}
		        },
		        "type": "ui_flow_block"
	        },
            container_id: pageId,
            position: { x: el.x, y: el.y }
        }
    }

    return [graph, ids];
}
