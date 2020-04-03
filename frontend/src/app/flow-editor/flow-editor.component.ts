
import {switchMap} from 'rxjs/operators';
import { Component, Input, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ProgramContent, FlowProgram, ProgramLogEntry, ProgramInfoUpdate } from '../program';
import { ProgramService } from '../program.service';

import * as progbar from '../ui/progbar';
import { buildBaseToolbox, Toolbox } from './toolbox'

import { FlowProgramSerializer } from './flow_program_serializer';
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

// import { ToolboxController } from '../blocks/ToolboxController';

@Component({
    selector: 'app-my-flow-editor',
    templateUrl: './flow-editor.component.html',
    providers: [
        ConnectionService, CustomBlockService, CustomSignalService,
        MonitorService, ProgramService, ServiceService, SessionService,
        TemplateService
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

    // toolboxController: ToolboxController;
    portraitMode: boolean;
    smallScreen: boolean;

    constructor(
        private monitorService: MonitorService,
        private programService: ProgramService,
        private customBlockService: CustomBlockService,
        private customSignalService: CustomSignalService,
        private route: ActivatedRoute,
        private router: Router,
        private dialog: MatDialog,
        private templateService: TemplateService,
        private serviceService: ServiceService,
        private notification: MatSnackBar,
        private connectionService: ConnectionService,
        private sessionService: SessionService,
    ) {
    }

    ngOnInit(): void {
        this.environment = environment;

        if (window && (window.innerWidth < window.innerHeight)) {
            this.portraitMode = true;
        } else {
            this.portraitMode = false;
        }
        this.smallScreen = window.innerWidth < 750;

        progbar.track(new Promise((resolve) => {
            this.sessionService.getSession()
                .then((session) => {
                    this.session = session;
                    this.route.params.pipe(
                        switchMap((params: Params) => {
                            this.programId = params['program_id'];
                            return this.programService.getProgramById(params['program_id']).catch(err => {
                                console.error("Error:", err);
                                this.goBack();
                                throw Error("Error loading");
                            });
                        }))
                        .subscribe(program => {
                            this.program = program;
                            resolve();
                            this.prepareWorkspace().then((/* controller: ToolboxController */) => {
                                // this.load_program(controller, program);
                                resolve();
                            }).catch(err => {
                                console.error("Error:", err);
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

    // /**
    //  * Check if an DOM element is a Scratch block object.
    //  */
    // is_block(blockCandidate: Element) {
    //     if (blockCandidate.tagName === undefined) {
    //         return false;
    //     }
    //     return blockCandidate.tagName.toUpperCase() === 'BLOCK';
    // }

    // /**
    //  * Clean a program in DOM format in-place.
    //  *
    //  * The resulting program doesn't contain any block that is not present
    //  *  on the Scratch Toolbox.
    //  *
    //  */
    // removeNonExistingBlocks(dom: Element, controller: ToolboxController)  {
    //     const children = dom.childNodes;
    //     for (let i = 0; i < children.length; i++) {
    //         const child = children[i] as Element;
    //         if (child.tagName !== 'block') {
    //             continue;
    //         }

    //         // Clean the contents of the block
    //         const next = child.getElementsByTagName('next')[0];
    //         let next_blocks = [];
    //         if (next !== undefined) {
    //             this.removeNonExistingBlocks(next, controller);

    //             next_blocks = (Array.from(next.childNodes)
    //                            .filter((x: Element) => this.is_block(x)));

    //             if (next_blocks.length == 0) {
    //                 child.removeChild(next);
    //                 continue;
    //             }
    //         }

    //         const _type = child.getAttribute('type');
    //         // Check if the current block
    //         if (!controller.isKnownBlock(_type)) {
    //             // If it's not known, pull the next into the top level "function"
    //             if (next !== undefined) {
    //                 next.removeChild(next_blocks[0]);
    //                 dom.insertBefore(next_blocks[0], child);
    //                 child.removeChild(next);
    //                 this.removeNonExistingBlocks(next, controller);
    //             }

    //             // Remove top level
    //             dom.removeChild(child);
    //             console.debug("To replace:", child, 'with', next);
    //         }
    //     }
    // }

    // load_program(controller: ToolboxController, program: ProgramContent) {
    //     const xml = Blockly.Xml.textToDom(program.orig);
    //     this.removeNonExistingBlocks(xml, controller);
    //     (Blockly.Xml as any).clearWorkspaceAndLoadFromXml(xml, this.workspace);

    //     this.initializeListeners();
    // }

    initializeListeners() {
        this.programService.watchProgramLogs(this.program.owner, this.program.id,
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

    async prepareWorkspace(): Promise<void /* ToolboxController */> {
        // For consistency and because it affects the positioning of the bottom drawer.
        this.reset_header_scroll();

        // return new Toolbox(
        //     this.monitorService,
        //     this.customBlockService,
        //     this.dialog,
        //     this.templateService,
        //     this.serviceService,
        //     this.customSignalService,
        //     this.connectionService,
        //     this.sessionService,
        // )
        //     .inject()
        //     .then(([toolbox, registrations, controller]) => {
        this.injectWorkspace(); // toolbox, registrations, controller);

        //     return controller;
        // });
    }

    injectWorkspace() {// toolbox: HTMLElement, registrations: Function[] /* , controller: ToolboxController */)
        const workspaceElement = document.getElementById('workspace');
        const programHeaderElement = document.getElementById('program-header');

        window.onresize = (() => {
            this.calculate_size(workspaceElement);
            this.calculate_program_header_size(programHeaderElement);
            this.workspace.onResize();
            this.toolbox.onResize();
        });
        this.calculate_size(workspaceElement);
        this.calculate_program_header_size(programHeaderElement);

        this.workspace = FlowWorkspace.BuildOn(workspaceElement);
        this.toolbox = buildBaseToolbox(workspaceElement, this.workspace);
        // this.workspace.drawSample();

        // for (const reg of registrations) {
        //     reg(this.workspace);
        // }

        // this.toolboxController = controller;
        // controller.setWorkspace(this.workspace);
        // controller.update();
    }

    calculate_size(workspace: HTMLElement) {
        const header = document.getElementById('program-header');
        if (!header) { return; }
        const header_pos = this.get_position(header);
        const header_end = header_pos.y + header.clientHeight;

        const window_height = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

        workspace.style.height = (window_height - header_end) + 'px';
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
        // this.router.navigate(['/dashboard'])
        return false;
    }

    dispose() {
    }

    async sendProgram(): Promise<void> {
        // // Get workspace
        // const xml = Blockly.Xml.workspaceToDom(this.workspace);

        // // Remove comments
        // for (const comment of Array.from(xml.getElementsByTagName('COMMENT'))) {
        //     comment.parentNode.removeChild(comment);
        // }

        // // Serialize result
        // const serializer = new FlowProgramSerializer(this.toolboxController);
        // const serialized = serializer.ToJson(xml);
        // const program = new FlowProgram(this.program,
        //                                 serialized.parsed,
        //                                 serialized.orig);

        // // Send update
        // const button = document.getElementById('program-start-button');
        // if (button){
        //     button.classList.add('started');
        //     button.classList.remove('completed');
        // }

        // const result = await this.programService.updateProgram(this.programUserName, program);

        // if (button){
        //     button.classList.remove('started');
        //     button.classList.add('completed');
        // }

        // return result;
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

            const update = (this.programService.updateProgramTags(this.program.owner, this.program.id, data.tags)
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

            const stopThreads = (this.programService.stopThreadsProgram(this.program.owner, this.program.id)
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
        window.dispatchEvent(new Event('resize'));
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
