import { Location, isPlatformServer } from '@angular/common';
import {switchMap} from 'rxjs/operators';
import { Component, Input, OnInit, ViewChild, Inject, NgZone, PLATFORM_ID } from '@angular/core';
import { ActivatedRoute, Params, Router } from '@angular/router';
import { ProgramContent, ScratchProgram, ProgramLogEntry, ProgramInfoUpdate, ProgramEditorEventValue } from './program';
import { ProgramService } from './program.service';

import { Toolbox, ToolboxRegistration } from './blocks/Toolbox';
import { BlockSynchronizer, BlocklyEvent } from './blocks/BlockSynchronizer';
import * as progbar from './ui/progbar';
/// <reference path="./blocks/blockly-core.d.ts" />
import ScratchProgramSerializer from './program_serialization/scratch-program-serializer';
import { MonitorService } from './monitor.service';
import { CustomBlockService } from './custom_block.service';

import { MatDialog } from '@angular/material/dialog';
import { MatDrawer } from '@angular/material/sidenav';

import { MatSnackBar } from '@angular/material/snack-bar';
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';
import { DeleteProgramDialogComponent } from './DeleteProgramDialogComponent';
import { StopThreadProgramDialogComponent } from './StopThreadProgramDialogComponent';
import { SetProgramTagsDialogComponent } from './program_tags/SetProgramTagsDialogComponent';
import { ToolboxController } from './blocks/ToolboxController';
import { TemplateService } from './templates/template.service';
import { ServiceService } from './service.service';
import { CustomSignalService } from './custom_signals/custom_signal.service';
import { ConnectionService } from './connection.service';
import { SessionService } from './session.service';
import { environment } from '../environments/environment';
import { unixMsToStr } from './utils';
import { Synchronizer } from './syncronizer';
import { MatMenu } from '@angular/material/menu';
import { AssetService } from './asset.service';
import { BrowserService } from './browser.service';
import { EditorController } from './program-editors/editor-controller';
import { Unsubscribable } from 'rxjs';
import { EnvironmentService } from './environment.service';
import { VisibilityEnum, ChangeProgramVisilibityDialog } from './dialogs/change-program-visibility-dialog/change-program-visibility-dialog.component';
import { CloneProgramDialogComponent } from './dialogs/clone-program-dialog/clone-program-dialog.component';
import { CloneProgramDialogComponentData } from './dialogs/clone-program-dialog/clone-program-dialog.component';
import { Session } from './session';

type NonReadyReason = 'loading' | 'disconnected';

@Component({
    selector: 'app-my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [
        AssetService, ConnectionService, CustomBlockService, CustomSignalService,
        MonitorService, ProgramService, ServiceService, SessionService,
        TemplateService
    ],
    styleUrls: [
        'program-detail.component.scss',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
})
export class ProgramDetailComponent implements OnInit {
    @Input() program: ProgramContent;
    workspace: Blockly.WorkspaceSvg;
    programId: string;
    environment: { [key: string]: any };
    session: Session;

    @ViewChild('logs_drawer') logs_drawer: MatDrawer;

    logs_drawer_initialized: boolean = false;
    commented_blocks: { [key:string]: [number, HTMLButtonElement]} = {};

    toolboxController: ToolboxController;
    portraitMode: boolean;
    smallScreen: boolean;
    patchedFunctions: {recordDeleteAreas: (() => void)} = { recordDeleteAreas: null };
    eventStream: Synchronizer<ProgramEditorEventValue>;
    isReady: boolean;
    connectionLost: boolean;
    private workspaceElement: HTMLElement;

    private cursorDiv: HTMLElement;
    private cursorInfo: {};
    nonReadyReason: NonReadyReason;
    logCount = 0;
    streamingLogs = false;

    read_only: boolean = true;
    can_admin: boolean = false;
    is_public: boolean = false;

    // HACK: Prevent the MatMenu import for being removed
    private _pinRequiredMatMenuLibrary: MatMenu;
    eventSubscription: Unsubscribable | null;
    logSubscription: Unsubscribable | null;
    blockSynchronizer: BlockSynchronizer;

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
        @Inject(PLATFORM_ID) private platformId: Object
    ) {
        this.monitorService = monitorService;
        this.programService = programService;
        this.customBlockService = customBlockService;
        this.customSignalService = customSignalService;
        this.route = route;
        this.router = router;
        this.serviceService = serviceService
        this.isReady = false;
        this.nonReadyReason = 'loading';

        this.cursorInfo = {};
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
                                throw Error("Error loading");
                            }
                            else {
                                console.error("Error:", err);
                                this.goBack();
                                throw Error("Error loading");
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
                                this.goBack();
                                throw Error("Error loading");
                            }
                        })
                    }
                }))
                .subscribe(program => {
                    this.programId = program.id;
                    this.read_only = program.readonly;
                    this.is_public = program.is_public;
                    this.can_admin = program.can_admin;

                    this.prepareWorkspace(program).then((controller: ToolboxController) => {
                        this.program = program;
                        this.load_program(controller, program);
                        resolve();
                    }).catch(err => {
                        console.error("Error:", err);
                        resolve();
                        this.goBack();
                    });
                });
        }));
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
            }

            const _type = child.getAttribute('type');
            // Check if the current block
            if (!controller.isKnownBlock(_type)) {
                // If it's not known, pull the next into the top level "function"
                if (next !== undefined) {
                    if (next_blocks.length > 0) {
                        next.removeChild(next_blocks[0]);
                        dom.insertBefore(next_blocks[0], child);
                    }
                    child.removeChild(next);
                    this.removeNonExistingBlocks(next, controller);
                }

                // Remove top level
                dom.removeChild(child);
            }
        }
    }

    load_program(controller: ToolboxController, program: ProgramContent) {
        let source = program.orig;
        if (program.checkpoint) {
            source = program.checkpoint;
        }
        const xml = Blockly.Xml.textToDom(source);
        this.removeNonExistingBlocks(xml, controller);
        (Blockly.Xml as any).clearWorkspaceAndLoadFromXml(xml, this.workspace);
    }

    becomeReady() {
        this.isReady = true;
    }

    initializeListeners() {
        // Initialize log listeners
        this.streamingLogs = true;
        if (!this.program.readonly) {
            this.logSubscription = this.programService.watchProgramLogs(this.program.id,
                                                                        { request_previous_logs: true })
                .subscribe(
                    {
                        next: (update: ProgramInfoUpdate) => {
                            if (update.type === 'program_log') {
                                this.updateLogsDrawer(update.value);
                                this.logCount++;
                            }
                        },
                        error: (error: any) => {
                            console.error("Error reading logs:", error);
                            this.streamingLogs = false;
                        },
                        complete: () => {
                            console.warn("No more logs about program", this.programId)
                            this.streamingLogs = false;
                        }
                    });
        }

        this.initializeEventSynchronization();
    }

    initializeEventSynchronization() {
        // Initialize editor event listeners
        // This is used for collaborative editing.

        if (this.read_only) {
            this.becomeReady(); // We won't have to wait for the last state to get loaded
            return;
        }

        this.eventStream = this.programService.getEventStream(this.program.id);
        this.blockSynchronizer = new BlockSynchronizer(this.eventStream, this.checkpointProgram.bind(this));

        const onCreation = {};
        const mirrorEvent = (event: BlocklyEvent) => {
            if (event instanceof Blockly.Events.Ui) {
                return;  // Don't mirror UI events.
            }

            if (this.blockSynchronizer.isDuplicated(event)) {
                return; // Avoid mirroring events received from the net
            }

            // Convert event to JSON.  This could then be transmitted across the net.
            const json = event.toJson();

            // Avoid passing messages about being created outside of this editor
            if (onCreation[json['blockId']]) {
                console.debug('Skipping sending message to block on creation');
                return;
            }

            try {
                if (this.isReady) {
                    this.eventStream.push({ type: 'blockly_event', value: json, save: true });
                }
            }
            catch (error) {
                console.log(error);
            }
        }

        this.eventSubscription = this.eventStream.subscribe(
            {
                next: (ev: ProgramEditorEventValue) => {
                    if (ev.type === 'blockly_event') {
                        const event = Blockly.Events.fromJson(ev.value, this.workspace);

                        this.blockSynchronizer.receivedEvent(event as BlocklyEvent);
                        if (ev.value.type === 'create') {
                            onCreation[ev.value.blockId] = true;
                        }
                        else if (ev.value.type === 'endDrag') {
                            delete onCreation[ev.value.blockId];
                        }

                        event.run(true);
                    }
                    else if (ev.type === 'cursor_event') {
                        this.drawPointer(ev.value);
                    }
                    else if (ev.type === 'add_editor') {
                        this.newPointer(ev.value.id);
                    }
                    else if (ev.type === 'remove_editor') {
                        this.deletePointer(ev.value.id);
                    }
                    else if (ev.type === 'ready') {
                        this.becomeReady();
                    }
                },
                error: (error: any) => {
                    console.error("Error obtainig editor events:", error);
                },
                complete: () => {
                    console.log("Disconnected");
                    this.nonReadyReason = 'disconnected';
                    this.isReady = false;
                }
            }
        );

        const onMouseEvent = ((ev: MouseEvent) => {
            if (ev.buttons) {
                return;
            }

            const disp = ProgramDetailComponent.getEditorPosition(this.workspaceElement);

            const rect = this.workspaceElement.getBoundingClientRect();
            const cursorInWorkspace = { x: ev.x - rect.left, y: ev.y - rect.top }

            const posInCanvas = {
                x: (cursorInWorkspace.x - disp.x) / disp.scale,
                y: (cursorInWorkspace.y - disp.y) / disp.scale,
            }

            this.eventStream.push({ type: 'cursor_event', value: posInCanvas })
        });

        this.workspace.addChangeListener(mirrorEvent);
        this.workspaceElement.onmousemove = onMouseEvent;
        this.workspaceElement.onmouseup = onMouseEvent;
    }

    /* Collaborator pointer management */
    newPointer(id: string): HTMLElement {
        const cursor = document.createElement('object');
        cursor.type = 'image/svg+xml';
        cursor.style.display = 'none';
        cursor.style.position = 'fixed';
        cursor.style.height = '2.5ex';
        cursor.style.color
        cursor.style.zIndex = '10';
        cursor.style.pointerEvents = 'none';
        cursor.data = '/assets/cursor.svg';
        cursor.onload = () => {
            // Give the cursor a random color
            cursor.getSVGDocument().getElementById('cursor').style.fill = `hsl(${Math.random() * 255},50%,50%)`;
        };

        this.cursorDiv.appendChild(cursor);
        this.cursorInfo[id] = cursor;

        return cursor;
    }

    getPointer(id: string): HTMLElement {
        if (this.cursorInfo[id]) {
            return this.cursorInfo[id];
        }

        return this.newPointer(id);
    }

    deletePointer(id: string) {
        const cursor = this.cursorInfo[id];
        if (!cursor) {
            return;
        }
        this.cursorDiv.removeChild(cursor);
        delete this.cursorInfo[id];
    }

    drawPointer(pos:{id: string, x : number, y: number}) {
        const rect = this.workspaceElement.getBoundingClientRect();
        const disp = ProgramDetailComponent.getEditorPosition(this.workspaceElement);
        const cursor = this.getPointer(pos.id);

        const posInScreen = {
            x: pos.x * disp.scale + disp.x + rect.left,
            y: pos.y * disp.scale + disp.y + rect.top,
        }
        cursor.style.left = posInScreen.x + 'px';
        cursor.style.top = posInScreen.y + 'px';

        let inEditor = false;
        if (rect.left <= posInScreen.x && rect.right >= posInScreen.x) {
            if (rect.top <= posInScreen.y && rect.bottom >= posInScreen.y) {
                inEditor = true;
            }
        }

        if (inEditor) {
            cursor.style.display = 'block';
        }
        else {
            cursor.style.display = 'none';
        }
    }

    checkpointProgram() {
        const xml = Blockly.Xml.workspaceToDom(this.workspace);

        // Remove comments
        for (const comment of Array.from(xml.getElementsByTagName('COMMENT'))) {
            comment.parentNode.removeChild(comment);
        }

        const content = Blockly.Xml.domToPrettyText(xml);

        return this.programService.checkpointProgram(this.program.id, content);
    }

    static getEditorPosition(workspaceElement: HTMLElement): {x:number, y: number, scale: number} | null {

        const SVG_TRANSFORM_TRANSLATE = 2;
        const SVG_TRANSFORM_SCALE = 3;

        let reference: HTMLElement | null = workspaceElement.getElementsByClassName('blocklySvg')[0].getElementsByClassName('blocklyBlockCanvas')[0] as HTMLElement;

        // Not dragging, so we can directly use Blockly's canvas as reference
        if (reference) {
            const transformations : SVGTransformList = (reference as any).transform.baseVal;
            let x = 0, y = 0, scale = 1;

            for (let i = 0; i < transformations.numberOfItems; i++) {
                const t = transformations[i];

                if (t.type === SVG_TRANSFORM_TRANSLATE) {
                    x = t.matrix.e;
                    y = t.matrix.f;
                }
                else if (t.type === SVG_TRANSFORM_SCALE) {
                    scale = t.matrix.a;
                }
            }

            return { x, y, scale};
        }

        // If we have to use the drag surface as refernce this is a bit less clean
        else {
            reference = workspaceElement.getElementsByClassName('blocklyWsDragSurface')[0] as HTMLElement;

            if (!reference) {
                console.error("Could not find reference");
                return null;
            }

            // Take transformation from the drag surface
            const result = /translate3d\((-?\d+)px, *(-?\d+)px, *-?\d+px\)/.exec(reference.style.transform);

            const x = parseInt(result[1]), y = parseInt(result[2]);

            // Take scale from it's inner
            const canvas = reference.getElementsByClassName('blocklyBlockCanvas')[0] as HTMLElement;

            let scale = 1;
            const transformations : SVGTransformList = (canvas as any).transform.baseVal;

            for (let i = 0; i < transformations.numberOfItems; i++) {
                const t = transformations[i];

                if (t.type === SVG_TRANSFORM_SCALE) {
                    scale = t.matrix.a;
                }
            }

            return { x, y, scale };
        }

    }

    patch_flyover_area_deletion() {
        if ((Blockly.WorkspaceSvg.prototype as any).recordDeleteAreas_.orig) {
            return;
        }

        this.patchedFunctions.recordDeleteAreas = (Blockly.WorkspaceSvg.prototype as any).recordDeleteAreas_;
        (Blockly.WorkspaceSvg.prototype as any).recordDeleteAreas_ = () => {
            this.patchedFunctions.recordDeleteAreas.bind(this.workspace)();

            // Disable toolbox delete area use trashcan for deletion
            const tbDelArea = (this.workspace as any).deleteAreaToolbox_;
            if (tbDelArea) {
                tbDelArea.left = -100;
                tbDelArea.top = -100;
                tbDelArea.width = 0;
                tbDelArea.height = 0;
            }
        }
        (Blockly.WorkspaceSvg.prototype as any).recordDeleteAreas_.orig = this.patchedFunctions.recordDeleteAreas;
    }

    async reloadToolbox(): Promise<any> {
        const toolbox = new Toolbox(
            this.program,
            this.assetService,
            this.monitorService,
            this.customBlockService,
            this.dialog,
            this.templateService,
            this.serviceService,
            this.customSignalService,
            this.connectionService,
            this.sessionService,
            this.environmentService,
            this.read_only,
            this.toolboxController,
        );

        const [toolboxXml, registrations, _controller] = await toolbox.inject();

        this.toolboxController.setToolbox(toolboxXml);
        this.toolboxController.update();
        this.performToolboxRegistrations(registrations);
    }

    prepareWorkspace(program: ProgramContent): Promise<ToolboxController> {
        // For consistency and because it affects the positioning of the bottom drawer.
        this.reset_header_scroll();

        return new Toolbox(
            program,
            this.assetService,
            this.monitorService,
            this.customBlockService,
            this.dialog,
            this.templateService,
            this.serviceService,
            this.customSignalService,
            this.connectionService,
            this.sessionService,
            this.environmentService,
            this.read_only,
        )
            .inject()
            .then(([toolbox, registrations, controller]) => {
                this.injectWorkspace(toolbox, registrations, controller);

                return controller;
            });
    }

    injectWorkspace(toolbox: HTMLElement, registrations: ToolboxRegistration[], controller: ToolboxController) {
        // Avoid initializing it twice
        if (this.workspace) {
            return;
        }

        this.cursorDiv = document.getElementById('program-cursors');

        this.workspaceElement = document.getElementById('workspace');
        const programHeaderElement = document.getElementById('program-header');

        this.hide_workspace(this.workspaceElement);
        this.browser.window.onresize = (() => {
            this.calculate_size(this.workspaceElement);
            this.calculate_program_header_size(programHeaderElement);
        });
        this.calculate_size(this.workspaceElement);
        this.calculate_program_header_size(programHeaderElement);
        const rtl = false;
        const soundsEnabled = false;
        let toolbarLayout = { horizontal: false, position: 'start' };
        if (this.portraitMode) {
            toolbarLayout = { horizontal: true, position: 'end'};
        }

        this.workspace = Blockly.inject('workspace', {
            comments: false,
            disable: false,
            collapse: true,
            media: '../assets/scratch-media/',
            readOnly: this.read_only,
            trashcan: true,
            rtl: rtl,
            scrollbars: true,
            toolbox: toolbox,
            toolboxPosition: toolbarLayout.position,
            horizontalLayout: toolbarLayout.horizontal,
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
        }) as Blockly.WorkspaceSvg;
        this.performToolboxRegistrations(registrations);

        this.toolboxController = controller;
        controller.setWorkspace(this.workspace);

        if (!this.read_only) {
            controller.update(); // Setting the toolbox when it can't be shown would generate an error
        }

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
            this.show_workspace(this.workspaceElement);

            // Listeners have to be started after the whole initialization is
            // done to avoid capturing the events happening during the start-up.
            this.initializeListeners();

            if (this.portraitMode || this.smallScreen){
                this.hide_block_menu();
                this.set_drawer_show_hide_flow();
            }

            this.reset_zoom();

            const dragContainer = document.querySelector('.blocklyBlockDragSurface>g');
            dragContainer.setAttribute('filter', 'drop-shadow(0 0 5px rgba(0,0,0,0.5))');

            if (this.portraitMode || this.smallScreen) {
                this.patch_flyover_area_deletion();
            }
        }, 0);

        this.patch_blockly();
    }

    performToolboxRegistrations(registrations: ToolboxRegistration[]) {
        const editorController: EditorController = {
            reloadToolbox: () => {
                this.reloadToolbox();
            }
        };
        for (const reg of registrations) {
            reg(this.workspace, editorController, this.ngZone);
        }
    }

    /**
     * Patch in changes made to adapt blockly/scratch to this use case.
     */
    patch_blockly() {
        // Patch show/hide variable (and list) blocks.
        // This blocks are not used (as of now) as the frontend does
        // not run the program and there's no point in showing
        // that in the background.
        (Blockly as any).DataCategory.addShowVariable = (_1: any, _2: any) => { };
        (Blockly as any).DataCategory.addHideVariable = (_1: any, _2: any) => { };
        (Blockly as any).DataCategory.addShowList = (_1: any, _2: any) => { };
        (Blockly as any).DataCategory.addHideList = (_1: any, _2: any) => { };

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

    reset_zoom() {
        // Procedure taken from Scratch's ZoomReset button
        // https://github.com/LLK/scratch-blocks/blob/4062a436c7111faf58385a0e16e30e3d7a5e6297/core/zoom_controls.js#L293
        (this.workspace as any).markFocused();
        (this.workspace as any).setScale((this.workspace as any).options.zoomOptions.startScale);
        (this.workspace as any).scrollCenter();
        Blockly.Touch.clearTouchIdentifier();  // Don't block future drags.
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

    set_drawer_show_hide_flow(): void {
        const component = this;

        // Add autoshow
        const categories = document.getElementsByClassName('scratchCategoryMenuItem');
        for (let i = 0; i < categories.length; i++) {
            const category = categories.item(i);

            const focus_category = (category as any).eventListeners()[0];

            const move_and_show = (() => {
                component.show_block_menu();

                try {
                    focus_category();
                } catch(ex) {
                    console.warn(ex);
                }

                return false;
            });

            (category as any).onclick = move_and_show;
            (category as any).ontouchend = move_and_show;
        }

        this.workspace.addChangeListener((event) => {
            if (event.type === Blockly.Events.BLOCK_CREATE) {
                component.hide_block_menu();
            }
        });
    }

    hide_block_menu() {
        (this.workspace as any).getFlyout().setVisible(false);
    }

    show_block_menu() {
        (this.workspace as any).getFlyout().setVisible(true);
    }

    show_workspace(workspace: HTMLElement) {
        workspace.style.visibility = 'visible';

        // Elements might have moved around.
        // We trigger a resize to notify SVG elements.
        this.browser.window.dispatchEvent(new Event('resize'));
    }

    hide_workspace(workspace: HTMLElement) {
        workspace.style.visibility = 'hidden';
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
        try {
            this.workspace.dispose();
            this.workspace = null;
        } catch(error) {
            console.error("Error disposing workspace:", error);
        }

        try {
            if (this.eventSubscription) {
                this.eventSubscription.unsubscribe();
                this.eventSubscription = null;
            }

            if (this.logSubscription) {
                this.logSubscription.unsubscribe();
                this.logSubscription = null;
            }

            if (this.blockSynchronizer) {
                this.blockSynchronizer.close();
                this.blockSynchronizer = null;
            }

            this.eventStream = null;
        } catch(error) {
            console.error("Error closing event stream:", error);
        }

        // Restore the patched function, to cleaup the state.
        try {
            if (this.patchedFunctions.recordDeleteAreas) {
                (Blockly.WorkspaceSvg.prototype as any).recordDeleteAreas_ = this.patchedFunctions.recordDeleteAreas;
                this.patchedFunctions.recordDeleteAreas = null;
            }
        } catch (error) {
            console.error("Error restoring recordDeleteAreas:", error);
        }
    }

    async sendProgram(): Promise<boolean> {
        // Get workspace
        const xml = Blockly.Xml.workspaceToDom(this.workspace);

        // Remove comments
        for (const comment of Array.from(xml.getElementsByTagName('COMMENT'))) {
            comment.parentNode.removeChild(comment);
        }

        // Serialize result
        const serializer = new ScratchProgramSerializer(this.toolboxController);
        const serialized = serializer.ToJson(xml);
        const program = new ScratchProgram(this.program,
                                           serialized.parsed,
                                           serialized.orig);

        // Send update
        const button = document.getElementById('program-start-button');
        if (button){
            button.classList.add('started');
            button.classList.remove('completed');
        }

        const result = await this.programService.updateProgram(program);

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

        // Get workspace
        const xml = Blockly.Xml.workspaceToDom(this.workspace);

        // Remove comments
        for (const comment of Array.from(xml.getElementsByTagName('COMMENT'))) {
            comment.parentNode.removeChild(comment);
        }

        // Serialize result
        const serializer = new ScratchProgramSerializer(this.toolboxController);
        const serialized = serializer.ToJson(xml);

        programData.program.orig = serialized.orig;
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
            this.router.navigate([`/programs/${program_id}/scratch`], { replaceUrl: false });
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
            const rename = (this.programService.renameProgram(this.program, programData.name)
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

    changeVisibility() {
        const visibility: VisibilityEnum = this.is_public ? 'public' : 'private';

        const data = {
            name: this.program.name,
            visibility
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
            let isPublic = undefined;
            switch(vis) {
                case 'public':
                    isPublic = true;
                    break;

                case 'private':
                    isPublic = false;
                    break;

                default:
                    throw Error(`Unknown visibility value: ${vis}`);
            }

            this.programService.updateProgramVisibility( this.program.id, { is_public: isPublic } ).then(() => {
                this.is_public = isPublic;
            });

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

            const deletion = (this.programService.deleteProgram(this.program)
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

        if (marked) { // Unmark
            button.innerText = 'Mark block';
            this.commented_blocks[log_line.block_id] = undefined;
            this.workspace.getBlockById(log_line.block_id).setCommentText(null);
        }
        else { // Mark block
            button.innerText = 'Unmark block';
            if (entry !== undefined) {
                entry[1].innerText = 'Mark block';
            }

            this.commented_blocks[log_line.block_id] = [log_line.event_time, button];
            this.workspace.getBlockById(log_line.block_id).setCommentText(log_line.event_message);
        }
    }
}
