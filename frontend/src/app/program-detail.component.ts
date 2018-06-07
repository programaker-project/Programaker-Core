import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { ProgramMetadata, ProgramContent } from './program';
import { ProgramService } from './program.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'app-my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [ProgramService]
})

export class ProgramDetailComponent implements OnInit {
    @Input() program: ProgramMetadata;
    currentFillingInput: string;

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
            .subscribe(program => this.program = program);

        this.currentFillingInput = '';
    }

    goBack(): void {
        this.location.back();
    }
}
