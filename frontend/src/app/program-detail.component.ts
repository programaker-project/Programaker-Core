import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { Location } from '@angular/common';
import { ProgramMetadata, ProgramContent, ScratchProgram } from './program';
import { ProgramService } from './program.service';
import 'rxjs/add/operator/switchMap';
import { Toolbox } from './blocks/Toolbox';
import { ContentType } from './content-type';
import * as progbar from './ui/progbar';
/// <reference path="./blocks/blockly-core.d.ts" />
import ScratchProgramSerializer from './program_serialization/scratch-program-serializer';
import { MonitorService } from './monitor.service';
import { CustomBlockService } from './custom_block.service';

import { MatDialog } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';
import { StopThreadProgramDialogComponent } from './StopThreadProgramDialogComponent';
import { SetProgramTagsDialogComponent } from './program_tags/SetProgramTagsDialogComponent';
import { ToolboxController } from './blocks/ToolboxController';
import { TemplateService } from './templates/template.service';
import { ServiceService } from './service.service';
import { CustomSignalService } from './custom_signals/custom_signal.service';

@Component({
    selector: 'app-my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [CustomBlockService, CustomSignalService, MonitorService, ProgramService, TemplateService, ServiceService],
    styleUrls: [
        'program-detail.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
})
export class ProgramDetailComponent implements OnInit {
    @Input() program: ProgramContent;
    currentFillingInput: string;
    workspace: Blockly.Workspace;
    programUserName: string;
    programId: string;

    toolboxController: ToolboxController;

    constructor(
        private monitorService: MonitorService,
        private programService: ProgramService,
        private customBlockService: CustomBlockService,
        private customSignalService: CustomSignalService,
        private route: ActivatedRoute,
        private location: Location,
        private router: Router,
        public dialog: MatDialog,
        private templateService: TemplateService,
        private serviceService: ServiceService,
        private notification: MatSnackBar,
    ) {
        this.monitorService = monitorService;
        this.programService = programService;
        this.customBlockService = customBlockService;
        this.customSignalService = customSignalService;
        this.route = route;
        this.location = location;
        this.router = router;
        this.serviceService = serviceService;
    }

    ngOnInit(): void {
        progbar.track(new Promise((resolve) => {
            this.route.params
                .switchMap((params: Params) => {
                    this.programUserName = params['user_id'];
                    this.programId = params['program_id'];
                    return this.programService.getProgram(params['user_id'], params['program_id']).catch(err => {
                        console.error("Error:", err);
                        this.goBack();
                        throw Error("Error loading");
                    });
                })
                .subscribe(program => {
                    this.prepareWorkspace().then((controller: ToolboxController) => {
                        this.program = program;
                        this.load_program(controller, program);
                        resolve();
                    }).catch(err => {
                        console.error("Error:", err);
                        this.goBack();
                    });
                });
        }));
        this.currentFillingInput = '';
    }

    /**
     * Check if an DOM element is a Scratch block object.
     */
    is_block(blockCandidate: Element) {
        if (blockCandidate.tagName === undefined) {
            return false;
        }
        return blockCandidate.tagName.toUpperCase() === 'BLOCK';
    }

    /**
     * Clean a program in DOM format in-place.
     *
     * The resulting program doesn't contain any block that is not present
     *  on the Scratch Toolbox.
     *
     */
    removeNonExistingBlocks(dom: Element, controller: ToolboxController)  {
        const children = dom.childNodes;
        for (let i = 0; i < children.length; i++) {
            const child = children[i] as Element;
            if (child.tagName !== 'block') {
                continue;
            }

            // Clean the contents of the block
            const next = child.getElementsByTagName('next')[0];
            let next_blocks = [];
            if (next !== undefined) {
                this.removeNonExistingBlocks(next, controller);

                next_blocks = (Array.from(next.childNodes)
                               .filter((x: Element) => this.is_block(x)));

                if (next_blocks.length == 0) {
                    child.removeChild(next);
                }
            }

            const _type = child.getAttribute('type');
            // Check if the current block
            if (!controller.isKnownBlock(_type)) {
                // If it's not known, pull the next into the top level "function"
                if (next !== undefined) {
                    next.removeChild(next_blocks[0]);
                    dom.insertBefore(next_blocks[0], child);
                    child.removeChild(next);
                    this.removeNonExistingBlocks(next, controller);
                }

                // Remove top level
                dom.removeChild(child);
                console.debug("To replace:", child, 'with', next);
            }
        }
    }

    load_program(controller: ToolboxController, program: ProgramContent) {
        const xml = Blockly.Xml.textToDom(program.orig);
        this.removeNonExistingBlocks(xml, controller);
        (Blockly.Xml as any).clearWorkspaceAndLoadFromXml(xml, this.workspace);
    }

    prepareWorkspace(): Promise<ToolboxController> {
        return new Toolbox(
            this.monitorService,
            this.customBlockService,
            this.dialog,
            this.templateService,
            this.serviceService,
            this.customSignalService,
        )
            .inject()
            .then(([toolbox, registrations, controller]) => {
                this.injectWorkspace(toolbox, registrations, controller);

                return controller;
            });
    }

    injectWorkspace(toolbox: HTMLElement, registrations: Function[], controller: ToolboxController) {
        // Avoid initializing it twice
        if (this.workspace !== undefined) {
            return;
        }


        const workspaceElement = document.getElementById('workspace');
        this.hide_workspace(workspaceElement);
        window.onresize = () => this.calculate_size(workspaceElement);
        this.calculate_size(workspaceElement);
        const rtl = false;
        const side = 'bottom';
        const soundsEnabled = false;

        this.workspace = Blockly.inject('workspace', {
            comments: false,
            disable: false,
            collapse: true,
            media: '../assets/scratch-media/',
            readOnly: false,
            trashcan: true,
            rtl: rtl,
            scrollbars: true,
            toolbox: toolbox,
            toolboxPosition: 'start',
            horizontalLayout: false,
            sounds: soundsEnabled,
            zoom: {
                controls: true,
                wheel: true,
                startScale: 0.75,
                maxScale: 4,
                minScale: 0.25,
                scaleSpeed: 1.1
            },
            colours: {
                fieldShadow: 'rgba(255, 255, 255, 0.3)',
                dragShadowOpacity: 0.6
            }
        });

        for (const reg of registrations) {
            reg(this.workspace);
        }

        this.toolboxController = controller;
        controller.setWorkspace(this.workspace);
        controller.update();

        // HACK:
        // Defer a hide action, this is to compsensate for (what feels like)
        // scratch deferring re-setting the visibility of the sidebar
        // after the creation.
        // As we trigger it from a timeout it'll get caught after
        // scratch does the re-set.
        //
        // This unconsistency would make make the screen flicker where the
        //  sidebar would be. To compensate for this we set the visibility
        //  of the workspace to 'hidden' until the process has finished.
        setTimeout(() => {
            this.show_workspace(workspaceElement);
        }, 0);

        this.patch_blockly();
    }

    /**
     * Patch in changes made to adapt blockly/scratch to this use case.
     */
    patch_blockly() {
        // Patch show/hide variable (and list) blocks.
        // This blocks are not used (as of now) as the frontend does
        // not run the program and there's no point in showing
        // that in the background.
        (Blockly as any).DataCategory.addShowVariable = (_1, _2) => { };
        (Blockly as any).DataCategory.addHideVariable = (_1, _2) => { };
        (Blockly as any).DataCategory.addShowList = (_1, _2) => { };
        (Blockly as any).DataCategory.addHideList = (_1, _2) => { };

        // Patch blockly.hideChaff to ignore events where
        // resize is produced by a soft-keyboard element
        // see https://github.com/LLK/scratch-blocks/issues/1345
        const originalHideChaff = Blockly.hideChaff;
        Blockly.hideChaff = (opt_allowToolbox: boolean) => {
            if ((document.activeElement as any).type === 'text') {
                // Skip this event as it was probably produced
                // by a soft-keyboard showing up
                return;
            }

            return originalHideChaff(opt_allowToolbox);
        }
    }

    calculate_size(workspace: HTMLElement) {
        const header = document.getElementById('program-header');
        if (!header) { return; }
        const header_pos = this.get_position(header);
        const header_end = header_pos.y + header.clientHeight;

        const window_height = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

        workspace.style.height = (window_height - header_end) + 'px';
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

    show_workspace(workspace: HTMLElement) {
        workspace.style.visibility = 'visible';

        // Elements might have moved around.
        // We trigger a resize to notify SVG elements.
        window.dispatchEvent(new Event('resize'));
    }

    hide_workspace(workspace: HTMLElement) {
        workspace.style.visibility = 'hidden';
    }

    goBack(): boolean {
        this.router.navigate(['/dashboard'])
        return false;
    }

    sendProgram() {
        const xml = Blockly.Xml.workspaceToDom(this.workspace);

        const serializer = new ScratchProgramSerializer(this.toolboxController);
        const serialized = serializer.ToJson(xml);
        const program = new ScratchProgram(this.program,
            serialized.parsed,
            serialized.orig);
        this.programService.updateProgram(this.programUserName, program);
    }

    renameProgram() {
        const programData = { name: this.program.name };

        const dialogRef = this.dialog.open(RenameProgramDialogComponent, {
            data: programData
        });

        dialogRef.afterClosed().subscribe(result => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const rename = (this.programService.renameProgram(this.programUserName, this.program, programData.name)
                .catch(() => { return false; })
                .then(success => {
                    if (!success) {
                        return;
                    }

                    this.program.name = programData.name;
                    const path = document.location.pathname.split("/");
                    path[path.length - 1] = encodeURIComponent(this.program.name);

                    this.router.navigate([path.join("/")]);
                    console.log("Changing name to", this.program);
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

            const deletion = (this.programService.deleteProgram(this.programUserName, this.program)
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
}
