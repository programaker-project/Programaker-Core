import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { ProgramMetadata } from './program';
import { ProgramService } from './program.service';

import { Session } from './session';
import { SessionService } from './session.service';

import { AvailableService } from './service';
import { ServiceService } from './service.service';

@Component({
    // moduleId: module.id,
    selector: 'app-my-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [ProgramService, SessionService, ServiceService],
    styleUrls: [
        'dashboard.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
})

export class DashboardComponent {
    programs: ProgramMetadata[] = [];
    services: AvailableService[] = [];
    session: Session = null;

    constructor(
        private programService: ProgramService,
        private serviceService: ServiceService,
        private sessionService: SessionService,
        private router: Router,
    ) {
        this.programService = programService;
        this.serviceService = serviceService;
        this.sessionService = sessionService;
        this.router = router;
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active) {
                    this.router.navigate(['/login']);
                } else {
                    this.programService.getPrograms()
                        .then(programs => this.programs = programs);

                    this.serviceService.getAvailableServices()
                        .then(services => this.services = services);
                }
            })
    }


    addProgram(): void {
        this.programService.createProgram().then(program => {
            this.openProgram(program);
        });
    }

    openProgram(program: ProgramMetadata): void {
        this.sessionService.getSession().then(session =>
            this.router.navigate([ '/users/' + session.username
                                 + '/programs/' + program.name]));
    }
}
