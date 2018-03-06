import { Component, OnInit } from '@angular/core';
import { Service, RequestInput, Request } from './service';
import { BotService } from './bot.service';
import { Location } from '@angular/common';
import { Http } from '@angular/http';
import { GetTypeOfJson, JSONType } from './json';
import { Router } from '@angular/router';
import { Bot } from './bot';
import { Program } from './program';
import { TelegramChannel } from './channel';
import { ServiceService } from './service.service';

import { ProgramServicePair } from './program_service_pair';
import { ProgramService } from './program.service';

import { Session } from './session';
import { SessionService } from './session.service';

@Component({
    // moduleId: module.id,
    selector: 'my-add-bots',
    templateUrl: './add-bot.component.html',
    providers: [BotService, ServiceService, ProgramService],
})


export class AddBotComponent {


    private createBotUrl = "https://wireup.hivemind.ai/api/bots/create";

    telegram: TelegramChannel;
    example: string;
    bot: Bot;
    programServicePairs: ProgramServicePair[];

    session: Session;
    programs: Program[];
    services: Service[];

    selectedProgram: Program;
    selectedService: Service;

    constructor(
        private location: Location,
        private http: Http,
        private router: Router,
        private programService: ProgramService,
        private serviceService: ServiceService,
        private sessionService: SessionService,
    ) {
        this.location = location;
        this.http = http;
        this.router = router;
        this.programService = programService;
        this.serviceService = serviceService;
        this.sessionService = sessionService;

        this.bot = { name: "", id: null };
        this.example = "";
        this.telegram = { token: "", activated: false };
        this.programServicePairs = [];

        this.programs = [];
        this.services = [];
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active){
                    this.router.navigate(['/login']);
                }
                else {
                    this.programService.getPrograms().then((programs) => this.programs = programs);
                    this.serviceService.getServices().then((services) => this.services = services);
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


    selectProgram(program: Program): void {
        this.selectedProgram = program;
    }


    selectService(service: Service): void {
        this.selectedService = service;
    }


    addProgramServicePair(): void {
        this.programServicePairs.push({ program: this.selectedProgram, service: this.selectedService });
        this.selectedProgram = undefined;
        this.selectedService = undefined;
    }


    create(): void {
        var buttons = document.getElementsByClassName("createButton");
        this.toIndeterminateProgressbar(buttons[0]);

        this.http.post(this.createBotUrl, { bot: this.bot,
                                            channels: { telegram: this.telegram },
                                            program_service_mapping: this.programServicePairs,
                                          })
            .subscribe(
                (response) => {
                    this.router.navigate(['/dashboard/']);
                },
                error => this.restoreFromIndeterminateProgressbar(buttons[0])
            );
    }
}
