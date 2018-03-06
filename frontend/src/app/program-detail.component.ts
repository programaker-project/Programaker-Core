import { Component, Input, OnInit } from '@angular/core';
import { ActivatedRoute, Params } from '@angular/router';
import { Location } from '@angular/common';
import { Program, ProgramExample } from './program';
import { ProgramService } from './program.service';
import 'rxjs/add/operator/switchMap';

@Component({
    selector: 'my-program-detail',
    templateUrl: './program-detail.component.html',
    providers: [ProgramService]
})

export class ProgramDetailComponent implements OnInit {
    @Input() program : Program;
    @Input() examples : ProgramExample[];
    currentFillingInput: string;

    ngOnInit(): void {
        this.route.params
            .switchMap((params: Params) => this.programService.getProgram(+params['id']))
            .subscribe(program => this.program = program);

        this.route.params
            .switchMap((params: Params) => this.programService.getExamplesForProgram(+params['id']))
            .subscribe(examples => this.examples = examples);

        this.currentFillingInput = "";
    }

    goBack(): void {
        this.location.back();
    }

    addExample(): void {
        var example = { id: null, text: this.currentFillingInput, failed: false };

        this.programService.persistExample(this.program, example)
            .then(persisted_example => example.id = persisted_example.id,
                  error => example.failed = true);

        this.currentFillingInput = "";
        this.examples.push(example);
    }

    constructor (
        private programService: ProgramService,
        private route: ActivatedRoute,
        private location: Location
    ) {
        this.programService = programService;
        this.route = route;
        this.location = location;

        this.examples = [];
    }
}
