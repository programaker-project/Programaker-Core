import * as progbar from '../ui/progbar';

import { HowToEnableServiceDialogComponent } from '../HowToEnableServiceDialogComponent';
import { AddConnectionDialogComponent } from '../connections/add-connection-dialog.component';

import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { ProgramMetadata } from '../program';
import { ProgramService } from '../program.service';

import { Session } from '../session';
import { SessionService } from '../session.service';

import { AvailableService, ServiceEnableHowTo } from '../service';
import { ServiceService } from '../service.service';
import { MatDialog } from '@angular/material/dialog';
import { MatSlideToggle } from '@angular/material/slide-toggle';
import { MatSlideToggleChange } from '@angular/material/slide-toggle';

import { MonitorMetadata } from '../monitor';
import { MonitorService } from '../monitor.service';
import { ConnectionService } from '../connection.service';
import { BridgeConnection, HashedIcon } from '../connection';
import { BridgeService } from '../bridges/bridge.service';
import { BridgeIndexData } from '../bridges/bridge';
import { BridgeDeleteDialogComponent } from '../bridges/delete-dialog.component';
import { ApiHost } from '../api-config';
import { iconDataToUrl } from '../utils';

type BridgeConnectionWithIconUrl = { conn: BridgeConnection, extra: { icon_url?: string}};

@Component({
    // moduleId: module.id,
    selector: 'app-my-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [BridgeService, ConnectionService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'dashboard.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class NewDashboardComponent {
    programs: ProgramMetadata[] = [];
    connections: BridgeConnectionWithIconUrl[] = null;
    bridgeIcons: { [key:string]: string} = {};
    session: Session = null;
    tutorials: { description: string, icons: string[] }[] = [
        // // Example data
        // {
        //     description: "Get weather prediction on the morning",
        //     icons: [ "/assets/icons/telegram_logo.png" ],
        // },
        // {
        //     description: "Greet when followed",
        //     icons: [ "/assets/icons/instagram_logo.png" ],
        // },
        // {
        //     description: "Add saved messages to Google Sheets",
        //     icons: [
        //         "/assets/icons/twitter_logo.png",
        //         "/assets/icons/google_sheets_logo.svg",
        //     ],
        // },
    ];

    constructor(
        private programService: ProgramService,
        private sessionService: SessionService,
        private serviceService: ServiceService,
        private connectionService: ConnectionService,
        private router: Router,
        public dialog: MatDialog,
        public bridgeService: BridgeService,
    ) {
        this.programService = programService;
        this.sessionService = sessionService;
        this.serviceService = serviceService;
        this.connectionService = connectionService;
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

                    this.connectionService.getConnections()
                        .then(connections => this.connections = connections.map((v, _i, _a) => {
                            const icon_url = iconDataToUrl(v.icon, v.bridge_id);


                            return { conn: v, extra: {icon_url: icon_url }};
                        }));

                    this.bridgeService.listUserBridges().then(bridgeData => {
                        for (const bridge of bridgeData){
                            this.bridgeIcons[bridge.id] = iconDataToUrl(bridge.icon, bridge.id);
                        }
                    });
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login']);
            })
    }

    addProgram(): void {
        this.programService.createProgram().then(program => {
            this.openProgram(program);
        });
    }

    addConnection(): void {
        const dialogRef = this.dialog.open(AddConnectionDialogComponent, { width: '90%' });

        dialogRef.afterClosed().subscribe(result => { });
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
