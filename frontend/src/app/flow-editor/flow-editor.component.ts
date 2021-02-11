import { Location, isPlatformServer } from '@angular/common';
import {switchMap} from 'rxjs/operators';
import { Component, Input, OnInit, ViewChild, Inject, PLATFORM_ID } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ProgramContent, ProgramLogEntry, ProgramInfoUpdate, ProgramType, VisibilityEnum } from '../program';
import { ProgramService } from '../program.service';

import * as progbar from '../ui/progbar';
import { Toolbox } from './toolbox'
import { fromCustomBlockService } from './toolbox_builder';

import { FlowWorkspace } from './flow_workspace';

import { CustomBlockService } from '../custom_block.service';

import { MatDialog } from '@angular/material/dialog';
import { MatDrawer } from '@angular/material/sidenav';

import { MatSnackBar } from '@angular/material/snack-bar';
import { RenameProgramDialogComponent } from '../RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from '../DeleteProgramDialogComponent';
import { StopThreadProgramDialogComponent } from '../StopThreadProgramDialogComponent';
import { SetProgramTagsDialogComponent } from '../program_tags/SetProgramTagsDialogComponent';
import { ServiceService } from '../service.service';
import { ConnectionService } from '../connection.service';
import { SessionService } from '../session.service';
import { unixMsToStr } from '../utils';
import { Session } from '../session';
import { FlowGraph } from './flow_graph';
import { EnumValue } from './enum_direct_value';
import { compile } from './graph_analysis';
import { BrowserService } from 'app/browser.service';
import { EnvironmentService } from 'app/environment.service';
import { UiSignalService } from 'app/services/ui-signal.service';
import { ContainerFlowBlock } from './ui-blocks/container_flow_block';
import { UI_ICON } from './definitions';
import { ResponsivePageBuilder, ResponsivePageGenerateTree } from './ui-blocks/renderers/responsive_page';
import { ChangeProgramVisilibityDialog } from '../dialogs/change-program-visibility-dialog/change-program-visibility-dialog.component';
import { CloneProgramDialogComponentData, CloneProgramDialogComponent } from '../dialogs/clone-program-dialog/clone-program-dialog.component';
import { uuidv4 } from './utils';
import { EnvironmentDefinition } from 'environments/environment-definition';
import { environment } from 'environments/environment';

@Component({
    selector: 'app-my-flow-editor',
    templateUrl: './flow-editor.component.html',
    styleUrls: [
        'flow-editor.component.scss',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class FlowEditorComponent implements OnInit {
    @Input() program: ProgramContent;
    @ViewChild('logs_drawer') logs_drawer: MatDrawer;

    session: Session;
    programId: string;
    environment: EnvironmentDefinition;
    workspace: FlowWorkspace;
    toolbox: Toolbox;

    logs_drawer_initialized: boolean = false;
    commented_blocks: { [key:string]: [number, HTMLButtonElement]} = {};

    portraitMode: boolean;
    smallScreen: boolean;
    pages: { name: string; url: string; }[];
    workspaceElement: HTMLElement;
    read_only: boolean = true;
    can_admin: boolean = false;
    visibility: VisibilityEnum;

    constructor(
        private browser: BrowserService,

        private programService: ProgramService,
        private customBlockService: CustomBlockService,
        private route: ActivatedRoute,
        private router: Router,
        private location: Location,
        private dialog: MatDialog,
        private serviceService: ServiceService,
        private notification: MatSnackBar,
        private connectionService: ConnectionService,
        private sessionService: SessionService,
        private uiSignalService: UiSignalService,
        private environmentService: EnvironmentService,
        @Inject(PLATFORM_ID) private platformId: Object
    ) {
    }

    ngOnInit(): Promise<void> {
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

        return progbar.track(new Promise((resolve, reject) => {
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
                                if (!session.active) {
                                    // Trying to read a program without a session, login
                                    this.router.navigate(['/login'], {replaceUrl: true});
                                    reject();
                                    throw Error("Error loading");
                                }
                                else {
                                    // Just go back
                                    // TODO: Show an appropriate error

                                    console.error("Error:", err);
                                    this.goBack();
                                    reject();
                                    throw Error("Error loading");
                                }
                            });
                        }))
                        .subscribe(program => {
                            this.program = program;
                            this.read_only = program.readonly;
                            this.visibility = program.visibility;
                            this.can_admin = program.can_admin;

                            this.prepareWorkspace().then(() => {
                                this.load_program(program);
                                resolve();
                            }).catch(err => {
                                console.error(err);
                                resolve();
                                // this.goBack();
                            });
                        });
                })
                .catch(err => {
                    console.error("Error loading program:", err);
                    reject();
                    this.goBack();
                });
        }));
    }

    load_program(program: ProgramContent) {
        if (program.orig && program.orig !== 'undefined') {
            this.workspace.load(program.orig as FlowGraph);

            console.time("Positioning");
            this.workspace.repositionIteratively().then(() => console.timeEnd("Positioning"));
        }
        else {
            this.workspace.initializeEmpty();
        }

        // For debugging
        (window as any).reposition = this.workspace.repositionAll.bind(this.workspace);
        (window as any).repositionIt = this.workspace.repositionIteratively.bind(this.workspace);

        this.workspace.center();

        this.initializeListeners();

        const pages = this.workspace.getPages();
        this.updateViewPages(Object.keys(pages));
    }

    addResponsivePage() {
        const block = new ContainerFlowBlock({
            icon: UI_ICON,
            type: 'ui_flow_block',
            subtype: 'container_flow_block',
            id: 'responsive_page_holder',
            builder: ResponsivePageBuilder,
            gen_tree: ResponsivePageGenerateTree,
            isPage: true,
        }, uuidv4(), this.uiSignalService);

        const blockId = this.workspace.draw(block);

        this.workspace.centerOnBlock(blockId);
    }

    updateViewPages(pages: string[]) {
        this.pages = pages.map(page => { return { name: page, url: this.programService.getPageUrl(this.programId, page) } });
    }

    openDefaultPage() {
        const url = this.programService.getPageUrl(this.programId, '/');
        let res = window.open(url,'_blank', 'noopener,noreferrer');
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
        this.workspaceElement = document.getElementById('workspace');
        const programHeaderElement = document.getElementById('program-header');

        this.browser.window.onresize = (() => {
            this.calculate_size(this.workspaceElement);
            this.calculate_program_header_size(programHeaderElement);
            this.workspace.onResize();
            this.toolbox.onResize();
        });
        this.calculate_size(this.workspaceElement);
        this.calculate_program_header_size(programHeaderElement);

        this.workspace = FlowWorkspace.BuildOn(this.workspaceElement,
                                               this.getEnumValues.bind(this),
                                               this.dialog,
                                               this.programId,
                                               this.programService,
                                               this.read_only,
                                               this.sessionService,
                                               this.environmentService,
                                              );
        this.toolbox = await fromCustomBlockService(this.workspaceElement, this.workspace,
                                                    this.customBlockService,
                                                    this.serviceService,
                                                    this.environmentService,
                                                    this.program.id,
                                                    this.uiSignalService,
                                                    this.connectionService,
                                                    this.session,
                                                    this.dialog,
                                                    this.reloadToolbox.bind(this),
                                                    this.read_only,
                                                   );
        this.workspace.setToolbox(this.toolbox);
    }

    async reloadToolbox() {
        const old = this.toolbox;
        this.toolbox = null;
        old.dispose();

        this.toolbox = await fromCustomBlockService(this.workspaceElement, this.workspace,
                                                    this.customBlockService,
                                                    this.serviceService,
                                                    this.environmentService,
                                                    this.program.id,
                                                    this.uiSignalService,
                                                    this.connectionService,
                                                    this.session,
                                                    this.dialog,
                                                    this.reloadToolbox.bind(this),
                                                    this.read_only,
                                                   );
        this.workspace.setToolbox(this.toolbox);
    }

    async getEnumValues(enum_namespace: string, enum_name: string): Promise<EnumValue[]> {
        if (enum_namespace === 'programaker') {
            if (enum_name === 'bridges') {
                const connections = await this.connectionService.getConnectionsOnProgram(this.programId);

                const knownBridges: {[key: string]: boolean} = {};
                const dropdown = [];
                for (const conn of connections) {
                    if (!knownBridges[conn.bridge_id]) {
                        knownBridges[conn.bridge_id] = true;
                        dropdown.push({ id: conn.bridge_id, name: conn.bridge_name } );
                    }
                }
                return dropdown;
            }
        }
        else {
            const values = await this.customBlockService.getCallbackOptions(this.program.id, enum_namespace, enum_name);

            return values.map(v => {
                return {
                    id: v[1], name: v[0],
                }
            });
        }
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
        this.location.back();
        return false;
    }

    dispose() {
        if (this.workspace) {
            this.workspace.dispose();
        }
        this.workspace = null;
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
            parsed: { blocks: compiled_program, variables: [] as [] },
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

    cloneProgram() {
        const programData: CloneProgramDialogComponentData = {
            name: this.program.name,
            program: JSON.parse(JSON.stringify(this.program)),
        };

        programData.program.orig = this.workspace.getGraph();
        if (((!programData.program.parsed) || (programData.program.parsed === 'undefined'))) {
            programData.program.parsed = { blocks: [], variables: [] };
        }

        const dialogRef = this.dialog.open(CloneProgramDialogComponent, {
            data: programData
        });

        dialogRef.afterClosed().subscribe(async (result) => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const program_id = result.program_id;
            this.dispose();
            this.router.navigate([`/programs/${program_id}/flow`], { replaceUrl: false });
        });
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

    changeVisibility() {
        const data = {
            name: this.program.name,
            visibility: this.visibility
        };

        const dialogRef = this.dialog.open(ChangeProgramVisilibityDialog, {
            data: data
        });


        dialogRef.afterClosed().subscribe((result: { visibility: VisibilityEnum } | null) => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const vis = result.visibility;
            this.programService.updateProgramVisibility( this.program.id, { visibility: vis } ).then(() => {
                this.visibility = vis;
            });

        });
    }

    setProgramTags() {
        const data = {
            program: this.program,
            user_id: this.program.owner,
            tags: [] as string[], // Initially empty, to be updated by dialog
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
