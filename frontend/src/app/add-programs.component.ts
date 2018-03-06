import { Component, OnInit } from '@angular/core';
import { Service, RequestInput, Request } from './service';
import { ProgramService } from './program.service';
import { Location } from '@angular/common';
import { Http } from '@angular/http';
import { GetTypeOfJson, JSONType } from './json';
import { Router } from '@angular/router';
import { Program } from './program';

import { Session } from './session';
import { SessionService } from './session.service';


@Component({
    // moduleId: module.id,
    selector: 'my-add-programs',
    templateUrl: './add-programs.component.html',
    providers: [ProgramService],
})

export class AddProgramsComponent {

    private createProgramUrl = "https://wireup.hivemind.ai/api/programs/create";

    session: Session;
    example: string;
    program: Program;

    constructor(
        private location: Location,
        private http: Http,
        private router: Router,
        private sessionService: SessionService,
    ) {
        this.location = location;
        this.http = http;
        this.router = router;
        this.sessionService = sessionService;

        this.program = { name: "", id: null };
        this.example = "";
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active){
                    this.router.navigate(['/login']);
                }
            });
    }

    goBack(): void {
        this.location.back();
    }

    toIndeterminateProgressbar(continueElement): void {
        continueElement.original = continueElement.innerHTML;
        continueElement.innerHTML = '...';
    }


    restoreFromIndeterminateProgressbar(continueElement): void {
        continueElement.innerHTML = continueElement.original;
    }


    create(): void {
        var buttons = document.getElementsByClassName("createButton");
        this.toIndeterminateProgressbar(buttons[0]);

        this.http.post(this.createProgramUrl, { program: this.program, example: this.example })
            .subscribe(
                (response) => {
                    this.router.navigate(['/programs/']);
                },
                error => this.restoreFromIndeterminateProgressbar(buttons[0])
            );
    }
}
