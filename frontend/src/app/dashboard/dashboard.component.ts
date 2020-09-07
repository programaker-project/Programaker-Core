import { Component } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { GroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { BridgeService } from '../bridges/bridge.service';
import { BridgeConnectionWithIconUrl } from '../connection';
import { ConnectionService } from '../connection.service';
import { AddConnectionDialogComponent } from '../connections/add-connection-dialog.component';
import { HowToEnableServiceDialogComponent } from '../HowToEnableServiceDialogComponent';
import { MonitorService } from '../monitor.service';
import { ProgramMetadata, ProgramType } from '../program';
import { ProgramService } from '../program.service';
import { SelectProgrammingModelDialogComponent } from '../programs/select-programming-model-dialog/select-programming-model-dialog.component';
import { AvailableService, ServiceEnableHowTo } from '../service';
import { ServiceService } from '../service.service';
import { Session } from '../session';
import { SessionService } from '../session.service';
import { iconDataToUrl } from '../utils';

type TutorialData = { description: string, icons: string[], url: string };

@Component({
    // moduleId: module.id,
    selector: 'app-my-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [BridgeService, ConnectionService, GroupService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'dashboard.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class NewDashboardComponent {
    programs: ProgramMetadata[] = [];
    connections: BridgeConnectionWithIconUrl[] = null;
    bridgeInfo: { [key:string]: { icon: string, name: string }} = {};
    session: Session = null;
    userInfo: {name: string, groups: GroupInfo[]};
    tutorials: TutorialData[] = [
        {
            description: "Create a weather chatbot",
            icons: [ "/assets/icons/telegram_logo.png", "/assets/icons/aemet_logo.png" ],
            url: "https://docs.programaker.com/tutorials/weather-bot.html",
        },
    ];

    constructor(
        private programService: ProgramService,
        private sessionService: SessionService,
        private serviceService: ServiceService,
        private connectionService: ConnectionService,
        private groupService: GroupService,
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
                    this.router.navigate(['/login'], {replaceUrl:true});
                } else {
                    this.userInfo = {
                        name: session.username,
                        groups: null
                    };
                    this.groupService.getUserGroups()
                        .then(groups => this.userInfo.groups = groups);
                    this.programService.getPrograms()
                        .then(programs => this.programs = programs);

                    this.updateConnections();
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            })
    }

    addProgram(): void {
        if (this.session.tags.is_in_preview) {
            const dialogRef = this.dialog.open(SelectProgrammingModelDialogComponent, { width: '90%' });

            dialogRef.afterClosed().subscribe((result: {success: boolean, program_type: ProgramType, program_name: string}) => {
                if (result && result.success) {
                    this.programService.createProgram(result.program_type, result.program_name).then(program => {
                        this.openProgram(program);
                    });
                }
            });
        }
        else {
            this.programService.createProgram('scratch_program').then(program => {
                this.openProgram(program);
            });
        }
    }

    addConnection(): void {
        const dialogRef = this.dialog.open(AddConnectionDialogComponent, { width: '90%' });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateConnections();
            }
        });
    }

    updateConnections(): void {
        this.connectionService.getConnections()
            .then(connections => {
                this.connections = connections.map((v, _i, _a) => {
                    const icon_url = iconDataToUrl(v.icon, v.bridge_id);

                    return { conn: v, extra: {icon_url: icon_url }};
                });

                for (const conn of connections){
                    this.bridgeInfo[conn.bridge_id] = {
                        icon: iconDataToUrl(conn.icon, conn.bridge_id),
                        name: conn.bridge_name
                    };
                }
            });
    }

    async openProgram(program: ProgramMetadata): Promise<void> {
        if (program.type === 'flow_program') {
            this.router.navigate(['/programs/' + program.id + '/flow']);
        }
        else if ((!program.type) || (program.type === 'scratch_program')) {
            const session = await this.sessionService.getSession();
            this.router.navigate(['/users/' + session.username
                                  + '/programs/' + encodeURIComponent(program.name)]);
        }
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

    async enableProgram(program: ProgramMetadata) {
        const session = await this.sessionService.getSession();
        await this.programService.setProgramStatus(JSON.stringify({"enable": true}),
                                             program.id,
                                             session.user_id);
        program.enabled = true;
    }

    openTutorial(tutorial: TutorialData) {
        const win = window.open(tutorial.url, '_blank');
        win.focus();
    }

    openGroup(group: GroupInfo) {
        this.router.navigateByUrl(`/groups/${group.canonical_name}`);

    }

    createGroup() {
        this.router.navigate(['/new/group']);
    }
}
