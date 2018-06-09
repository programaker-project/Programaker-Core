import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { ProgramMetadata, ProgramContent } from './program';
import { ProgramService } from './program.service';
import 'rxjs/add/operator/switchMap';
import { load_initial } from './blocks/initial';

@Component({
    selector: 'app-my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [ProgramService],
    styleUrls: [
        'program-detail.component.css'
    ],
})

export class ProgramDetailComponent implements OnInit {
    @Input() program: ProgramContent;
    currentFillingInput: string;
    workspace: any;

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
                return this.programService.getProgram(params['user_id'], params['program_id']);
            })
            .subscribe(program => {
                this.program = program;
                this.prepareWorkspace();
            });

        this.currentFillingInput = '';
    }

    prepareWorkspace() {
        // Avoid initializing it twice
        if (this.workspace !== undefined) {
            return;
        }

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

        const workspace = document.getElementById('workspace')
        window.onresize = () => this.calculate_size(workspace);
        this.calculate_size(workspace);

        load_initial(this.workspace);
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


    goBack(): boolean {
        history.go(-1);
        return false;
    }
}
