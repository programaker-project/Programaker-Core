import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
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
import { ChatService } from './chat.service';
import { Chat } from './chat';

import { MatDialog } from '@angular/material/dialog';
import { RenameProgramDialogComponent } from './RenameProgramDialogComponent';

@Component({
    selector: 'app-my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [MonitorService, ProgramService, ChatService],
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
    programUserId: string;
    menuBlockHidden: boolean;

    HIDDEN_BACKGROUND_COLOR = '#888';
    HIDDEN_BORDER_COLOR = '#222';
    HIDDEN_TEXT_LABEL = 'Show/Hide';

    constructor (
      private monitorService: MonitorService,
      private programService: ProgramService,
      private chatService: ChatService,
      private route: ActivatedRoute,
      private location: Location,
      public dialog: MatDialog,
  ) {
      this.monitorService = monitorService;
      this.programService = programService;
      this.route = route;
      this.location = location;
  }

    ngOnInit(): void {
        progbar.track(new Promise((resolve) => {
            this.route.params
                .switchMap((params: Params) => {
                    this.programUserId = params['user_id'];
                    return this.programService.getProgram(params['user_id'], params['program_id']);
                })
                .subscribe(program => {
                    this.prepareWorkspace().then(() => {
                        this.program = program;
                        this.load_program(program);
                        resolve();
                    });
                });
        }));
        this.currentFillingInput = '';
    }

    load_program(program: ProgramContent) {
        const xml = Blockly.Xml.textToDom(program.orig);
        Blockly.Xml.domToWorkspace(xml, this.workspace);
    }

    prepareWorkspace(): Promise<void> {
        return this.chatService.getAvailableChats().then((chats: Chat[]) => {
            return new Toolbox(this.monitorService, chats).inject().then(() => { this.injectWorkspace(); });
        });
    }

    injectWorkspace() {
        // Avoid initializing it twice
        if (this.workspace !== undefined) {
            return;
        }

        const workspaceElement = document.getElementById('workspace');
        this.hide_workspace(workspaceElement);
        window.onresize = () => this.calculate_size(workspaceElement);
        this.calculate_size(workspaceElement);

        const rtl = false;
        const toolbox = null;
        const side = 'bottom';
        const soundsEnabled = false;

        this.workspace = Blockly.inject('workspace', {
            comments: false,
            disable: false,
            collapse: true,
            media: '../media/',
            readOnly: false,
            rtl: rtl,
            scrollbars: false,
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

        this.add_show_hide_block_menu();

        // HACK#1
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
            this.hide_block_menu();
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
        (Blockly as any).DataCategory.addShowVariable = (_1, _2) => {};
        (Blockly as any).DataCategory.addHideVariable = (_1, _2) => {};
        (Blockly as any).DataCategory.addShowList = (_1, _2) => {};
        (Blockly as any).DataCategory.addHideList = (_1, _2) => {};

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
        const header_pos = this.get_position(header);
        const header_end = header_pos.y + header.clientHeight;

        const window_height = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

        workspace.style.height = (window_height - header_end) + 'px';
    }

    get_position(element: any): {x: number, y: number} {
        let xPosition = 0;
        let yPosition = 0;

        while (element) {
            xPosition += (element.offsetLeft - element.scrollLeft + element.clientLeft);
            yPosition += (element.offsetTop - element.scrollTop + element.clientTop);
            element = element.offsetParent;
        }

        return { x: xPosition, y: yPosition };
    }

    add_show_hide_block_menu(): void {
        const menu = document.getElementsByClassName('scratchCategoryMenu')[0];

        // Unselect element
        const selected = menu.getElementsByClassName('categorySelected');
        for (let i = 0; i < selected.length; i++) {
            selected[i].className = selected[i].className.replace(/\bcategorySelected\b/, '').trim();
        }

        // Add a new element
        const hideButton = document.createElement('div');
        hideButton.className = 'scratchCategoryMenuRow';
        hideButton.onclick = () => this.toggle_block_menu();

        const menuItem = document.createElement('div');
        menuItem.className = 'scratchCategoryMenuItem';

        const itemBubble = document.createElement('div');
        itemBubble.className = 'scratchCategoryItemBubble';
        itemBubble.style.backgroundColor = this.HIDDEN_BACKGROUND_COLOR;
        itemBubble.style.borderColor = this.HIDDEN_BORDER_COLOR;

        const itemLabel = document.createElement('div');
        itemLabel.className = 'scratchCategoryMenuItemLabel';
        itemLabel.innerText = this.HIDDEN_TEXT_LABEL;

        menuItem.appendChild(itemBubble);
        menuItem.appendChild(itemLabel);

        hideButton.appendChild(menuItem);
        menu.insertBefore(hideButton, menu.children[0]);
    }

    hide_block_menu() {
        this.menuBlockHidden = true;
        Array.from(document.getElementsByClassName('blocklyFlyout'))
            .forEach(e => {
                (e as HTMLElement).style.display = 'none';
            });
        Array.from(document.getElementsByClassName('blocklyFlyoutScrollbar'))
            .forEach(e => {
                (e as HTMLElement).style.display = 'none';
            });
    }

    show_block_menu() {
        this.menuBlockHidden = false;
        Array.from(document.getElementsByClassName('blocklyFlyout'))
            .forEach(e => {
                (e as HTMLElement).style.display = 'block';
            });
        Array.from(document.getElementsByClassName('blocklyFlyoutScrollbar'))
            .forEach(e => {
                (e as HTMLElement).style.display = 'block';
            });
    }

    toggle_block_menu() {
        if (this.menuBlockHidden) {
            this.show_block_menu();
        } else {
            this.hide_block_menu();
        }
    }

    show_workspace(workspace: HTMLElement) {
        workspace.style.visibility = 'visible';
    }

    hide_workspace(workspace: HTMLElement) {
        workspace.style.visibility = 'hidden';
    }

    goBack(): boolean {
        history.go(-1);
        return false;
    }

    sendProgram() {
        const xml = Blockly.Xml.workspaceToDom(this.workspace);

        const serialized = ScratchProgramSerializer.ToJson(xml);
        const program = new ScratchProgram(this.program,
                                            serialized.parsed,
                                            serialized.orig);
        this.programService.updateProgram(this.programUserId, program);
    }

    renameProgram() {
        const dialogRef = this.dialog.open(RenameProgramDialogComponent, {
            data: this.program
        });

        dialogRef.afterClosed().subscribe(result => {});

    }
}
