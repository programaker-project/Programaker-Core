import { Location, isPlatformServer } from '@angular/common';
import {switchMap} from 'rxjs/operators';
import { Component, Input, OnInit, ViewChild, Inject, PLATFORM_ID } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ProgramContent, FlowProgram, ProgramLogEntry, ProgramInfoUpdate, ProgramType } from '../program';
import { ProgramService } from '../program.service';

import * as progbar from '../ui/progbar';
import { Toolbox } from './toolbox'
import { fromCustomBlockService } from './toolbox_builder';

import { FlowWorkspace } from './flow_workspace';

import { MonitorService } from '../monitor.service';
import { CustomBlockService } from '../custom_block.service';

import { MatDialog } from '@angular/material/dialog';
import { MatDrawer } from '@angular/material/sidenav';

import { MatSnackBar } from '@angular/material/snack-bar';
import { RenameProgramDialogComponent } from '../RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from '../DeleteProgramDialogComponent';
import { StopThreadProgramDialogComponent } from '../StopThreadProgramDialogComponent';
import { SetProgramTagsDialogComponent } from '../program_tags/SetProgramTagsDialogComponent';
import { TemplateService } from '../templates/template.service';
import { ServiceService } from '../service.service';
import { CustomSignalService } from '../custom_signals/custom_signal.service';
import { ConnectionService } from '../connection.service';
import { SessionService } from '../session.service';
import { environment } from '../../environments/environment';
import { unixMsToStr } from '../utils';
import { Session } from '../session';
import { BridgeService } from '../bridges/bridge.service';
import { FlowGraph } from './flow_graph';
import { EnumValue } from './enum_direct_value';
import { compile } from './graph_analysis';
import { BrowserService } from 'app/browser.service';
import { EnvironmentService } from 'app/environment.service';
import { UiSignalService } from 'app/services/ui-signal.service';

@Component({
    selector: 'app-my-flow-editor',
    templateUrl: './flow-editor.component.html',
    providers: [
        BridgeService,
        ConnectionService, CustomBlockService, CustomSignalService,
        MonitorService, ProgramService, ServiceService, SessionService,
        TemplateService, UiSignalService,
    ],
    styleUrls: [
        'flow-editor.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class FlowEditorComponent implements OnInit {
    @Input() program: ProgramContent;
    @ViewChild('logs_drawer') logs_drawer: MatDrawer;

    session: Session;
    programId: string;
    environment: { [key: string]: any };
    workspace: FlowWorkspace;
    toolbox: Toolbox;

    logs_drawer_initialized: boolean = false;
    commented_blocks: { [key:string]: [number, HTMLButtonElement]} = {};

    portraitMode: boolean;
    smallScreen: boolean;
    pages: { name: string; url: string; }[];

    constructor(
        private browser: BrowserService,

        private monitorService: MonitorService,
        private programService: ProgramService,
        private customBlockService: CustomBlockService,
        private customSignalService: CustomSignalService,
        private route: ActivatedRoute,
        private router: Router,
        private _location: Location,
        private dialog: MatDialog,
        private templateService: TemplateService,
        private serviceService: ServiceService,
        private notification: MatSnackBar,
        private connectionService: ConnectionService,
        private sessionService: SessionService,
        private bridgeService: BridgeService,
        private uiSignalService: UiSignalService,
        private environmentService: EnvironmentService,
        @Inject(PLATFORM_ID) private platformId: Object
    ) {
    }

    ngOnInit(): void {
        this.environment = environment;

        if (isPlatformServer(this.platformId)) {
            // This cannot be rendered on server, so halt it's load
            return;
        }

        if (this.browser.window && (this.browser.window.innerWidth < this.browser.window.innerHeight)) {
            this.portraitMode = true;
        } else {
            this.portraitMode = false;
        }
        this.smallScreen = this.browser.window.innerWidth < 750;

        progbar.track(new Promise((resolve) => {
            this.sessionService.getSession()
                .then((session) => {
                    this.session = session;
                    this.route.params.pipe(
                        switchMap((params: Params) => {
                            this.programId = params['program_id'];

                            // Note that configuring the UiSignal this way means
                            // that it can be in a semi-initialized state, which
                            // is not good. This should be fixed in the future
                            // if we still need this same data.
                            this.uiSignalService.setProgramId(this.programId);

                            return this.programService.getProgramById(params['program_id']).catch(err => {
                                console.error("Error:", err);
                                this.goBack();
                                throw Error("Error loading");
                            });
                        }))
                        .subscribe(program => {
                            this.program = program;
                            this.prepareWorkspace().then(() => {
                                this.load_program(program);
                                resolve();
                            }).catch(err => {
                                console.error(err);
                                resolve();
                                this.goBack();
                            });
                        });
                })
                .catch(err => {
                    console.error("Error loading program:", err);
                    this.goBack();
                });
        }));
    }

    load_program(program: ProgramContent) {
        if (program.orig && program.orig !== 'undefined') {
            this.workspace.load(program.orig as FlowGraph);
        }

        this.initializeListeners();

        const pages = this.workspace.getPages();
        this.updateViewPages(Object.keys(pages));
    }

    updateViewPages(pages: string[]) {
        this.pages = pages.map(page => { return { name: page, url: this.programService.getPageUrl(this.programId, page) } });
    }

    initializeListeners() {
        this.programService.watchProgramLogs(this.program.id,
                                             { request_previous_logs: true })
            .subscribe(
                {
                    next: (update: ProgramInfoUpdate) => {
                        if (update.type === 'program_log') {
                            this.updateLogsDrawer(update.value);
                        }
                    },
                    error: (error: any) => {
                        console.error("Error reading logs:", error);
                    },
                    complete: () => {
                        console.log("No more logs about program", this.programId)
                    }
                });
    }

    async prepareWorkspace(): Promise<void> {
        // For consistency and because it affects the positioning of the bottom drawer.
        this.reset_header_scroll();

        await this.injectWorkspace();
    }

    async injectWorkspace() {
        const workspaceElement = document.getElementById('workspace');
        const programHeaderElement = document.getElementById('program-header');

        this.browser.window.onresize = (() => {
            this.calculate_size(workspaceElement);
            this.calculate_program_header_size(programHeaderElement);
            this.workspace.onResize();
            this.toolbox.onResize();
        });
        this.calculate_size(workspaceElement);
        this.calculate_program_header_size(programHeaderElement);

        this.workspace = FlowWorkspace.BuildOn(workspaceElement, this.getEnumValues.bind(this));
        this.toolbox = await fromCustomBlockService(workspaceElement, this.workspace,
                                                    this.customBlockService,
                                                    this.bridgeService,
                                                    this.environmentService,
                                                    this.program.id,
                                                    this.uiSignalService,
                                                    this.session,
                                                   );
        this.workspace.setToolbox(this.toolbox);
    }

    async getEnumValues(enum_namespace: string, enum_name: string): Promise<EnumValue[]> {
        const values = await this.customBlockService.getCallbackOptions(this.program.id, enum_namespace, enum_name);

        return values.map(v => {
            return {
                id: v[1], name: v[0],
            }
        });
    }

    calculate_size(workspace: HTMLElement) {
        const header = document.getElementById('program-header');
        if (!header) { return; }
        const header_pos = this.get_position(header);
        const header_end = header_pos.y + header.clientHeight;

        const window_height = Math.max(document.documentElement.clientHeight, this.browser.window.innerHeight || 0);

        workspace.style.height = (window_height - header_end - 1) + 'px';
    }

    calculate_program_header_size(programHeader: HTMLElement) {
        const isScrollable = programHeader.clientHeight < programHeader.scrollHeight;
        if (!isScrollable) {
            programHeader.classList.remove('is-scrollable');
        }
        else {
            programHeader.classList.add('is-scrollable');
        }
    }

    get_position(element: any): { x: number, y: number } {
        let xPosition = 0;
        let yPosition = 0;

        while (element) {
            xPosition += (element.offsetLeft - element.scrollLeft + element.clientLeft);
            yPosition += (element.offsetTop - element.scrollTop + element.clientTop);
            element = element.offsetParent;
        }

        return { x: xPosition, y: yPosition };
    }

    reset_header_scroll() {
        document.getElementById('program-header').scrollTo(0, 0);
    }

    goBack(): boolean {
        this.dispose();
        this._location.back();
        return false;
    }

    dispose() {
    }

    async sendProgram(): Promise<boolean> {
        const graph = this.workspace.getGraph();
        const pages = this.workspace.getPages();
        this.updateViewPages(Object.keys(pages));

        const t0 = new Date();
        const compiled_program = compile(graph);
        console.debug('Compilation time:', (new Date() as any) - (t0 as any), 'ms')

        // Send update
        const button = document.getElementById('program-start-button');
        if (button){
            button.classList.add('started');
            button.classList.remove('completed');
        }

        const program = {
            type: 'flow_program' as ProgramType,
            parsed: { blocks: compiled_program, variables: [] },
            pages: pages,
            orig: graph,
            id: this.programId,
        };

        const result = await this.programService.updateProgramById(program);

        if (button){
            button.classList.remove('started');
            button.classList.add('completed');
        }

        return result;
    }

    renameProgram() {
        const programData = { name: this.program.name };

        const dialogRef = this.dialog.open(RenameProgramDialogComponent, {
            data: programData
        });

        dialogRef.afterClosed().subscribe(async (result) => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            await this.sendProgram();
            const rename = (this.programService.renameProgramById(this.program.id, programData.name)
                .catch(() => { return false; })
                .then(success => {
                    if (!success) {
                        return;
                    }

                    this.notification.open('Renamed successfully', 'ok', {
                        duration: 5000
                    });
                }));
            progbar.track(rename);
        });
    }

    setProgramTags() {
        const data = {
            program: this.program,
            user_id: this.program.owner,
            tags: [], // Initially empty, to be updated by dialog
        };

        const dialogRef = this.dialog.open(SetProgramTagsDialogComponent, {
            data: data
        });

        dialogRef.afterClosed().subscribe(result => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const update = (this.programService.updateProgramTags(this.program.id, data.tags)
                            .then((success) => {
                                if (!success) {
                                    return;
                                }

                                this.notification.open('Tags updated', 'ok', {
                                    duration: 5000
                                });
                            })
                            .catch((error) => {
                                console.error(error);

                                this.notification.open('Error updating tags', 'ok', {
                                    duration: 5000
                                });
                            }));
            progbar.track(update);
        });
    }

    stopThreadsProgram() {
        const programData = { name: this.program.name };
        const dialogRef = this.dialog.open(StopThreadProgramDialogComponent, {
            data: programData
        });

        dialogRef.afterClosed().subscribe(result => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const stopThreads = (this.programService.stopThreadsProgram(this.program.id)
                .catch(() => { return false; })
                .then(success => {
                    if (!success) {
                        return;
                    }
                    this.notification.open('All Threads stopped', 'ok', {
                      duration: 5000
                    });
                }));
            progbar.track(stopThreads);
        });
    }

    deleteProgram() {
        const programData = { name: this.program.name };

        const dialogRef = this.dialog.open(DeleteProgramDialogComponent, {
            data: programData
        });

        dialogRef.afterClosed().subscribe(result => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const deletion = (this.programService.deleteProgramById(this.program.id)
                .catch(() => { return false; })
                .then(success => {
                    if (!success) {
                        return;
                    }

                    this.goBack();
                }));
            progbar.track(deletion);
        });
    }

    toggleLogsPanel() {
        if (this.logs_drawer.opened) {
            this.closeLogsPanel();
        }
        else {
            this.openLogsPanel();
        }
    }

    notifyResize() {
        this.browser.window.dispatchEvent(new Event('resize'));
    }

    closeLogsPanel() {
        this.logs_drawer.close().then(() => {
            // Notify Scratch containers
            this.notifyResize();
        });
    }

    openLogsPanel() {
        this.logs_drawer.open().then(() => {
            // Notify Scratch containers
            this.notifyResize();
        });
    }

    updateLogsDrawer(line: ProgramLogEntry) {
        const container = document.getElementById('logs_panel_container');
        if (!this.logs_drawer_initialized) {
            container.innerHTML = ''; // Clear container

            this.logs_drawer_initialized = true;
        }

        const newLine = this.renderLogLine(line);
        container.appendChild(newLine);

        if (this.logs_drawer.opened) {
            newLine.scrollIntoView();
        }
    }

    renderLogLine(line: ProgramLogEntry): HTMLElement {
        const element = document.createElement('div');
        element.classList.add('log-entry');

        const line_time = document.createElement('span');
        line_time.classList.add('time');
        line_time.innerText = unixMsToStr(line.event_time);

        element.appendChild(line_time);

        const message = document.createElement('span');
        message.classList.add('message');
        message.innerText = line.event_message;

        element.appendChild(message);

        if (line.block_id) {
            const mark_button = document.createElement('button');
            mark_button.classList.value = 'log-marker mat-button mat-raised-button mat-button-base mat-primary';

            mark_button.innerText = 'Mark block';
            mark_button.onclick = () => {
                this.toggleMark(mark_button, line);
            }

            element.appendChild(mark_button);
        }

        return element;
    }

    toggleMark(button: HTMLButtonElement, log_line: ProgramLogEntry) {
        const entry = this.commented_blocks[log_line.block_id];
        const marked = (entry !== undefined) && (entry[0] == log_line.event_time);

        // if (marked) { // Unmark
        //     button.innerText = 'Mark block';
        //     this.commented_blocks[log_line.block_id] = undefined;
        //     this.workspace.getBlockById(log_line.block_id).setCommentText(null);
        // }
        // else { // Mark block
        //     button.innerText = 'Unmark block';
        //     if (entry !== undefined) {
        //         entry[1].innerText = 'Mark block';
        //     }

        //     this.commented_blocks[log_line.block_id] = [log_line.event_time, button];
        //     this.workspace.getBlockById(log_line.block_id).setCommentText(log_line.event_message);
        // }
    }
}
