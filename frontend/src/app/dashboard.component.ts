import * as progbar from './ui/progbar';

import { HowToEnableServiceDialogComponent } from './HowToEnableServiceDialogComponent';

import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { ProgramMetadata } from './program';
import { ProgramService } from './program.service';

import { Session } from './session';
import { SessionService } from './session.service';

import { AvailableService, ServiceEnableHowTo } from './service';
import { ServiceService } from './service.service';
import { MatDialog } from '@angular/material/dialog';
import { MatSlideToggle, MatSlideToggleChange } from '@angular/material/slide-toggle';

import { MonitorMetadata } from './monitor';
import { MonitorService } from './monitor.service';
import { BridgeService } from './bridges/bridge.service';
import { BridgeIndexData } from './bridges/bridge';
import { BridgeDeleteDialogComponent } from './bridges/delete-dialog.component';

@Component({
    // moduleId: module.id,
    selector: 'app-my-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [BridgeService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'dashboard.component.css',
        'libs/css/material-icons.css',
        'libs/css/bootstrap.min.css',
    ],
})

export class DashboardComponent {
    programs: ProgramMetadata[] = [];
    services: AvailableService[] = [];
    monitors: MonitorMetadata[] = [];
    session: Session = null;

    constructor(
        private programService: ProgramService,
        private serviceService: ServiceService,
        private monitorService: MonitorService,
        private sessionService: SessionService,
        private router: Router,
        public dialog: MatDialog,
    ) {
        this.programService = programService;
        this.serviceService = serviceService;
        this.monitorService = monitorService;
        this.sessionService = sessionService;
        this.router = router;
    }

    // tslint:disable-next-line:use-life-cycle-interface
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

                    this.monitorService.getMonitors()
                        .then(monitors => this.monitors = monitors);
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login']);
            })
    }

    onToggleChange(ob: MatSlideToggleChange,program_id: string) {
        this.sessionService.getSession().then(session => {
            this.programService.toggleStatusProgram(ob.checked,program_id,session.user_id)
        });
    } 

    addProgram(): void {
        this.programService.createProgram().then(program => {
            this.openProgram(program);
        });
    }

    openProgram(program: ProgramMetadata): void {
        this.sessionService.getSession().then(session =>
            this.router.navigate(['/users/' + session.username
                + '/programs/' + encodeURIComponent(program.name)]));
    }

    enableService(service: AvailableService): void {
        this.serviceService.getHowToEnable(service)
            .then(howToEnable => this.showHowToEnable(howToEnable));
    }

    showHowToEnable(howTo: ServiceEnableHowTo): void {
        if ((howTo as any).success === false) {
            return;
        }
        const dialogRef = this.dialog.open(HowToEnableServiceDialogComponent, {
            data: howTo
        });

        dialogRef.afterClosed().subscribe(result => { });
    }
}
