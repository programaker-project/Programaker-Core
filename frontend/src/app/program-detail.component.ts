import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { ProgramMetadata, ProgramContent, ScratchProgram } from './program';
import { ProgramService } from './program.service';
import 'rxjs/add/operator/switchMap';
import { load_initial } from './blocks/initial';
import { ContentType } from './content-type';
/// <reference path="./blocks/blockly-core.d.ts" />
import ScratchProgramSerializer from './program_serialization/scratch-program-serializer';

@Component({
    selector: 'app-my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [ProgramService],
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
      private programService: ProgramService,
      private route: ActivatedRoute,
      private location: Location
  ) {
      this.programService = programService;
      this.route = route;
      this.location = location;
  }

    ngOnInit(): void {
        this.route.params
            .switchMap((params: Params) => {
                this.programUserId = params['user_id'];
                return this.programService.getProgram(params['user_id'], params['program_id']);
            })
            .subscribe(program => {
                this.prepareWorkspace();
                this.program = program;
                this.load_program(program);
            });

        this.currentFillingInput = '';
    }

    load_program(program: ProgramContent) {
        const xml = Blockly.Xml.textToDom(program.orig);
        Blockly.Xml.domToWorkspace(xml, this.workspace);
    }

    prepareWorkspace() {
        // Avoid initializing it twice
        if (this.workspace !== undefined) {
            return;
        }

        const workspaceElement = document.getElementById('workspace')
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
        this.hide_block_menu();
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
}
