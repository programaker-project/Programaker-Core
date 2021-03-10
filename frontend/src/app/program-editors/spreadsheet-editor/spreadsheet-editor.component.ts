import { isPlatformServer, Location } from '@angular/common';
import { AfterViewInit, Component, Inject, Input, NgZone, OnInit, PLATFORM_ID, ViewChild, ElementRef } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatDrawer } from '@angular/material/sidenav';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';
import { Unsubscribable } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { environment } from '../../../environments/environment';
import { AssetService } from '../../asset.service';
import { BlockSynchronizer } from '../../blocks/BlockSynchronizer';
import { ToolboxRegistration } from '../../blocks/Toolbox';
import { ToolboxController } from '../../blocks/ToolboxController';
import { BrowserService } from '../../browser.service';
import { ProgramEditorSidepanelComponent } from '../../components/program-editor-sidepanel/program-editor-sidepanel.component';
import { ConnectionService } from '../../connection.service';
import { CustomBlockService } from '../../custom_block.service';
import { CustomSignalService } from '../../custom_signals/custom_signal.service';
import { DeleteProgramDialogComponent } from '../../DeleteProgramDialogComponent';
import { ChangeProgramVisilibityDialog } from '../../dialogs/change-program-visibility-dialog/change-program-visibility-dialog.component';
import { EnvironmentService } from '../../environment.service';
import { MonitorService } from '../../monitor.service';
import { ProgramContent, ProgramEditorEventValue, VisibilityEnum } from '../../program';
import { ProgramService } from '../../program.service';
import { SetProgramTagsDialogComponent } from '../../program_tags/SetProgramTagsDialogComponent';
import { RenameProgramDialogComponent } from '../../RenameProgramDialogComponent';
import { ServiceService } from '../../service.service';
import { Session } from '../../session';
import { SessionService } from '../../session.service';
import { StopThreadProgramDialogComponent } from '../../StopThreadProgramDialogComponent';
import { Synchronizer } from '../../syncronizer';
import { TemplateService } from '../../templates/template.service';
import * as progbar from '../../ui/progbar';
import { MatMenu } from '@angular/material/menu';


@Component({
    selector: 'app-my-spreadsheet-editor',
    templateUrl: './spreadsheet-editor.component.html',
    providers: [
        AssetService, ConnectionService, CustomBlockService, CustomSignalService,
        MonitorService, ProgramService, ServiceService, SessionService,
        TemplateService
    ],
    styleUrls: [
        'spreadsheet-editor.component.scss',
        '../../libs/css/material-icons.css',
        '../../libs/css/bootstrap.min.css',
    ],
})
export class SpreadsheetEditorComponent implements OnInit, AfterViewInit {
    @Input() program: ProgramContent;
    workspace: Blockly.WorkspaceSvg;
    programId: string;
    environment: { [key: string]: any };
    session: Session;

    @ViewChild('drawer') drawer: MatDrawer;
    @ViewChild('sidepanel') sidepanel: ProgramEditorSidepanelComponent;
    @ViewChild('floatingEditor') floatingEditor: ElementRef<HTMLInputElement>;

    portraitMode: boolean;
    smallScreen: boolean;
    patchedFunctions: {recordDeleteAreas: (() => void)} = { recordDeleteAreas: null };
    eventStream: Synchronizer<ProgramEditorEventValue>;
    connectionLost: boolean;

    read_only: boolean = true;
    can_admin: boolean = false;

    // State
    cellValues: {[key:string]: string} = {};
    private cursorDiv: HTMLElement;
    private cursorInfo: {[key: string]: HTMLElement};
    activeCells: HTMLTableDataCellElement[] = [];
    current: HTMLTableDataCellElement;

    // HACK: Prevent the MatMenu import for being removed
    private _pinRequiredMatMenuLibrary: MatMenu;
    eventSubscription: Unsubscribable | null;
    mutationObserver: MutationObserver | null;
    blockSynchronizer: BlockSynchronizer;
    visibility: VisibilityEnum;


    constructor(
        private browser: BrowserService,
        private monitorService: MonitorService,
        private programService: ProgramService,
        private customBlockService: CustomBlockService,
        private customSignalService: CustomSignalService,
        private assetService: AssetService,
        private route: ActivatedRoute,
        private router: Router,
        private _location: Location,
        private templateService: TemplateService,
        private serviceService: ServiceService,
        private notification: MatSnackBar,
        private dialog: MatDialog,
        private connectionService: ConnectionService,
        private sessionService: SessionService,
        private ngZone: NgZone,
        private environmentService: EnvironmentService,
        private toastr: ToastrService,

        @Inject(PLATFORM_ID) private platformId: Object
    ) {
        this.cursorInfo = {};
        console.log("Cell", this.cellValues);
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

        progbar.track(new Promise(async (resolve, reject) => {

            this.session = await this.sessionService.getSession();

            this.route.params.pipe(
                switchMap((params: Params) => {
                    const user = params['user_id'];
                    if (user) {
                        const programName = params['program_id'];

                        return this.programService.getProgram(user, programName).catch(err => {
                            if (!this.session.active) {
                                this.router.navigate(['/login'], {replaceUrl:true});
                                reject();
                                this.toastr.error(err.message, "Error loading");
                                throw Error("Error loading");
                            }
                            else {
                                console.error("Error:", err);
                                this.toastr.error(err.message, "Error loading");
                                return null;
                            }
                        });
                    }
                    else {
                        const programId = params['program_id'];
                        return this.programService.getProgramById(programId).catch(err => {
                            if (!this.session.active) {
                                this.router.navigate(['/login'], {replaceUrl:true});
                                reject();
                                throw Error("Error loading");
                            }
                            else {
                                console.error("Error:", err);
                                this.toastr.error(err.message, "Error loading");
                                return null;
                            }
                        })
                    }
                }))
                .subscribe(program => {
                    if (program === null) {
                        return;
                    }

                    this.programId = program.id;
                    this.read_only = program.readonly;
                    this.visibility = program.visibility;
                    this.can_admin = program.can_admin;

                    // this.prepareWorkspace(program).then((controller: ToolboxController) => {
                    //     this.program = program;
                    //     this.load_program(controller, program);
                    //     resolve();
                    // }).catch(err => {
                    //     console.error("Error:", err);
                    //     resolve();
                    //     this.toastr.error(JSON.stringify(err), "Error loading");
                    // });
                });
        }));
    }

    ngAfterViewInit() {
        const elem = (this.drawer as any)._elementRef.nativeElement;

        this.mutationObserver = new MutationObserver(() => {
            this.notifyResize();

            // HACK: Wait for animations to finish
            for (let delay = 200; delay < 1000; delay *= 2 ) {
                setTimeout(() => {
                    this.notifyResize();
                }, delay);
            }
        });
        this.mutationObserver.observe(elem, { attributes: true, subtree: true  });
    }

    load_program(controller: ToolboxController, program: ProgramContent) {
        // let source = program.orig;
        // if (program.checkpoint) {
        //     source = program.checkpoint;
        // }
        // const xml = Blockly.Xml.textToDom(source);
        // this.removeNonExistingBlocks(xml, controller);
        // (Blockly.Xml as any).clearWorkspaceAndLoadFromXml(xml, this.workspace);
    }

    initializeListeners() {
        this.initializeEventSynchronization();
    }

    initializeEventSynchronization() {
    }

    checkpointProgram() {
    }

    // static getEditorPosition(workspaceElement: HTMLElement): {x:number, y: number, scale: number} | null {
    // }

    patch_flyover_area_deletion() {
    }

    async reloadToolbox(): Promise<any> {
    }

    injectWorkspace(toolbox: HTMLElement, registrations: ToolboxRegistration[], controller: ToolboxController) {
    }

    calculate_size(workspace: HTMLElement) {
        const header = document.getElementById('program-header');
        if (!header) { return; }
        // const header_pos = this.get_position(header);
        // const header_end = header_pos.y + header.clientHeight;

        // const window_height = Math.max(document.documentElement.clientHeight, this.browser.window.innerHeight || 0);

        // workspace.style.height = (window_height - header_end - 1) + 'px';
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

    reset_zoom() {
    }

    // get_position(element: any): { x: number, y: number } {
    // }

    reset_header_scroll() {
        document.getElementById('program-header').scrollTo(0, 0);
    }

    goBack(): boolean {
        this.dispose();
        this._location.back();
        return false;
    }

    force_reload() {
        location = location;
    }

    dispose() {
        // try {
        //     if (this.eventSubscription) {
        //         this.eventSubscription.unsubscribe();
        //         this.eventSubscription = null;
        //     }

        //     if (this.sidepanel) {
        //         this.sidepanel.dispose();
        //     }

        //     if (this.mutationObserver) {
        //         this.mutationObserver.disconnect();
        //         this.mutationObserver = null;
        //     }

        //     if (this.blockSynchronizer) {
        //         this.blockSynchronizer.close();
        //         this.blockSynchronizer = null;
        //     }

        //     this.eventStream = null;
        // } catch(error) {
        //     console.error("Error closing event stream:", error);
        // }

        // try {
        //     this.workspace.dispose();
        //     this.workspace = null;
        // } catch(error) {
        //     console.error("Error disposing workspace:", error);
        // }

        // // Restore the patched function, to cleaup the state.
        // try {
        //     if (this.patchedFunctions.recordDeleteAreas) {
        //         (Blockly.WorkspaceSvg.prototype as any).recordDeleteAreas_ = this.patchedFunctions.recordDeleteAreas;
        //         this.patchedFunctions.recordDeleteAreas = null;
        //     }
        // } catch (error) {
        //     console.error("Error restoring recordDeleteAreas:", error);
        // }
    }

    // async sendProgram(): Promise<boolean> {
    //     // Get workspace
    //     const xml = Blockly.Xml.workspaceToDom(this.workspace);

    //     // Remove comments
    //     for (const comment of Array.from(xml.getElementsByTagName('COMMENT'))) {
    //         comment.parentNode.removeChild(comment);
    //     }

    //     // Serialize result
    //     let program;
    //     try {
    //         const serializer = new ScratchProgramSerializer(this.toolboxController);
    //         const serialized = serializer.ToJson(xml);
    //         program = new ScratchProgram(this.program,
    //                                      serialized.parsed,
    //                                      serialized.orig);
    //     }
    //     catch (error) {
    //         this.toastr.error(error, 'Invalid program', {
    //             closeButton: true,
    //             progressBar: true,
    //         });

    //         console.error(error);
    //         return;
    //     }

    //     // Send update
    //     const button = document.getElementById('program-start-button');
    //     if (button){
    //         button.classList.add('started');
    //         button.classList.remove('completed');
    //     }

    //     const result = await this.programService.updateProgram(program);

    //     if (button){
    //         button.classList.remove('started');
    //         button.classList.add('completed');
    //     }

    //     if (result) {
    //         this.toastr.success('Upload complete', '', {
    //             closeButton: true,
    //             progressBar: true,
    //         });
    //     }
    //     else {
    //         this.toastr.error('Error on upload', '', {
    //             closeButton: true,
    //             progressBar: true,
    //         });
    //     }

    //     return result;
    // }

    cloneProgram() {
        // const programData: CloneProgramDialogComponentData = {
        //     name: this.program.name,
        //     program: JSON.parse(JSON.stringify(this.program)),
        // };

        // // Get workspace
        // const xml = Blockly.Xml.workspaceToDom(this.workspace);

        // // Remove comments
        // for (const comment of Array.from(xml.getElementsByTagName('COMMENT'))) {
        //     comment.parentNode.removeChild(comment);
        // }

        // // Serialize result
        // const serializer = new ScratchProgramSerializer(this.toolboxController);
        // const serialized = serializer.ToJson(xml);

        // programData.program.orig = serialized.orig;
        // if (((!programData.program.parsed) || (programData.program.parsed === 'undefined'))) {
        //     programData.program.parsed = { blocks: [], variables: [] };
        // }

        // const dialogRef = this.dialog.open(CloneProgramDialogComponent, {
        //     data: programData
        // });

        // dialogRef.afterClosed().subscribe(async (result) => {
        //     if (!result) {
        //         console.log("Cancelled");
        //         return;
        //     }

        //     const program_id = result.program_id;
        //     this.dispose();
        //     this.router.navigate([`/programs/${program_id}/scratch`], { replaceUrl: false });
        // });
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

            // await this.sendProgram();
            const rename = (this.programService.renameProgram(this.program, programData.name)
                .catch(() => { return false; })
                .then((success: boolean) => {
                    if (!success) {
                        return;
                    }

                    this.program.name = programData.name;
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
                .then((success: boolean) => {
                    if (!success) {
                        return;
                    }

                    this.notification.open('Tags updated', 'ok', {
                        duration: 5000
                    });
                })
                .catch((error: Error) => {
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
                .then((success: boolean) => {
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

            const deletion = (this.programService.deleteProgram(this.program)
                .catch(() => { return false; })
                .then((success: boolean) => {
                    if (!success) {
                        return;
                    }

                    this.goBack();
                }));
            progbar.track(deletion);
        });
    }

    toggleLogsPanel() {
        if (this.drawer.opened && this.sidepanel.drawerType === 'logs') {
            this.closeDrawer();
        }
        else {
            this.sidepanel.setDrawerType('logs');
            if (!this.drawer.opened) {
                this.openDrawer();
            }
        }
    }

    toggleVariablesPanel() {
        if (this.drawer.opened && this.sidepanel.drawerType === 'variables') {
            this.closeDrawer();
        }
        else {
            this.sidepanel.setDrawerType('variables');
            if (!this.drawer.opened) {
                this.openDrawer();
            }
        }
    }

    notifyResize() {
        this.browser.window.dispatchEvent(new Event('resize'));
    }

    openDrawer() {
        return this.drawer.open();
    }

    closeDrawer = () => {
        return this.drawer.close();
    }

    onToggleMark = (blockId: string, activate: boolean, message: string) => {
        if (activate) {
            this.workspace.getBlockById(blockId).setCommentText(message);
        }
        else {
            this.workspace.getBlockById(blockId).setCommentText(null);
        }
    }

    // Template helpers
    seq(start: number, end: number): number[] {
        if (start >= end) {
            return [];
        }

        return Array.from({length: end - start}, (_v, idx) => idx + start);
    }

    startResize(ev: MouseEvent) {
        const ref = (ev.target as HTMLElement).parentElement;
        window.onmousemove = (move: MouseEvent) => {
            ref.style.minWidth = move.clientX - ref.getBoundingClientRect().left + 'px';
        }

        window.onmouseup = () => {
            window.onmousemove = window.onmouseup = null;
        }
    }

    mousedownOnCell(ev: MouseEvent) {
        const elem = ev.target as HTMLTableDataCellElement;

        if (elem === this.current) {
            // Just treat it as text
            return;
        }

        const editor = this.floatingEditor.nativeElement;
        if (this.current && editor.value.trim().startsWith('=')) {
            // Selecting other cell while on formula
            ev.preventDefault();

            const val = this.getCellId(elem);
            editor.value = editor.value.substr(0, editor.selectionStart) + val[0] + ':' + val[1] + editor.value.substr(editor.selectionStart)

            return;
        }

        // Changing the current cell
        this.unsetActive();
        elem.classList.add('active');
        this.activeCells = [elem];
        this.makeCurrent(elem);
    }

    makeCurrent(elem: HTMLTableDataCellElement) {
        this.current = elem;
        const toRect = this.current.getBoundingClientRect();
        const editor = this.floatingEditor.nativeElement;
        editor.value = elem.innerText;

        const workspace = document.getElementById('workspace');
        const wsRect = workspace.getBoundingClientRect();

        editor.classList.remove('hidden');
        editor.classList.remove('editing');

        editor.style.left = toRect.left - wsRect.left + 'px';
        editor.style.top = toRect.top - wsRect.top + 'px';
        editor.style.minHeight = toRect.height + 'px';
        editor.style.width = toRect.width + 'px';

        setTimeout(() => editor.focus(), 0);
    }

    getCellId(elem: HTMLTableDataCellElement): [string, number] {
        const [_, row, col] = elem.id.split('_');

        return [col, parseInt(row)];
    }

    unsetActive() {
        const editor = this.floatingEditor.nativeElement;
        if (this.current) {
            this.current.innerText = editor.value;
        }
        editor.value = '';

        for (const cell of this.activeCells) {
            cell.classList.remove('active');
        }
    }
}
