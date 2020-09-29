import { Component } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { BridgeIndexData, SharedResource } from 'app/bridges/bridge';
import { AddCollaboratorsDialogComponent } from 'app/dialogs/add-collaborators-dialog/add-collaborators-dialog.component';
import { GroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Collaborator, CollaboratorRole, roleToIcon } from 'app/types/collaborator';
import { getGroupPictureUrl, iconDataToUrl } from 'app/utils';
import { BridgeService } from '../bridges/bridge.service';
import { BridgeConnectionWithIconUrl } from '../connection';
import { ConnectionService } from '../connection.service';
import { AddConnectionDialogComponent } from '../connections/add-connection-dialog.component';
import { MonitorService } from '../monitor.service';
import { ProgramMetadata, ProgramType } from '../program';
import { ProgramService } from '../program.service';
import { SelectProgrammingModelDialogComponent } from '../programs/select-programming-model-dialog/select-programming-model-dialog.component';
import { ServiceService } from '../service.service';
import { Session } from '../session';
import { SessionService } from '../session.service';
import { AddBridgeDialogComponent } from 'app/dialogs/add-bridge-dialog/add-bridge-dialog.component';
import { UpdateBridgeDialogComponent } from 'app/dialogs/update-bridge-dialog/update-bridge-dialog.component';

@Component({
    selector: 'app-group-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [BridgeService, ConnectionService, GroupService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'dashboard.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class GroupDashboardComponent {
    programs: ProgramMetadata[] = [];
    connections: BridgeConnectionWithIconUrl[] = null;
    bridgeInfo: { [key:string]: { icon: string, name: string }} = {};
    session: Session = null;
    groupInfo: GroupInfo;
    collaborators: Collaborator[] = null;
    userRole: CollaboratorRole = null;
    bridges: BridgeIndexData[] = null;
    sharedResources: SharedResource[];

    canWriteToGroup: boolean = false;

    readonly _roleToIcon = roleToIcon;
    readonly _getGroupPicture = getGroupPictureUrl;
    readonly _iconDataToUrl = iconDataToUrl;

    constructor(
        private programService: ProgramService,
        private sessionService: SessionService,
        private serviceService: ServiceService,
        private connectionService: ConnectionService,
        private groupService: GroupService,
        private router: Router,
        private route: ActivatedRoute,
        public dialog: MatDialog,
        public bridgeService: BridgeService,
    ) {
        this.programService = programService;
        this.sessionService = sessionService;
        this.serviceService = serviceService;
        this.connectionService = connectionService;
        this.router = router;
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(async (session) => {
                this.session = session;

                if (!session.active) {
                    this.router.navigate(['/login'], {replaceUrl:true});
                } else {
                    try {
                        const params = this.route.params['value'];
                        const groupName = params.group_name;

                        this.groupInfo = await this.groupService.getGroupWithName(groupName);

                        this.programService.getProgramsOnGroup(this.groupInfo.id)
                            .then(programs => {
                                this.programs = programs;
                            });

                        this.updateCollaborators();
                        this.updateBridges();
                        this.updateConnections();
                        this.updateSharedResources();
                    }
                    catch (err) {
                        console.error(err)
                    }
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
                    this.programService.createProgramOnGroup(result.program_type, result.program_name, this.groupInfo.id).then(program => {
                        this.openProgram(program);
                    });
                }
            });
        }
        else {
            this.programService.createProgramOnGroup('scratch_program', null, this.groupInfo.id).then(program => {
                this.openProgram(program);
            });
        }
    }

    addConnection(): void {
        const dialogRef = this.dialog.open(AddConnectionDialogComponent, { width: '90%', data: {groupId: this.groupInfo.id} });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateConnections();
            }
        });
    }

    addCollaborators(): void {
        const dialogRef = this.dialog.open(AddCollaboratorsDialogComponent, { width: '50%',
                                                                              data: { groupId: this.groupInfo.id,
                                                                                      existingCollaborators: this.collaborators,
                                                                                    },
                                                                            });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateCollaborators();
            }
        });
    }

    addBridge(): void {
        const dialogRef = this.dialog.open(AddBridgeDialogComponent, { width: '50%',
                                                                       data: { groupId: this.groupInfo.id,
                                                                             },
                                                                     });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateBridges();
            }
        });
    }

    updateConnections(): void {
        this.connectionService.getConnectionsOnGroup(this.groupInfo.id)
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

    async updateBridges() {
        this.bridges = await this.bridgeService.listGroupBridges(this.groupInfo.id);
    }

    async updateSharedResources() {
        this.sharedResources = await this.groupService.getSharedResources(this.groupInfo.id);
        console.log("Shared", this.sharedResources);
    }

    openBridgePanel(bridge: BridgeIndexData) {
        const dialogRef = this.dialog.open(UpdateBridgeDialogComponent, { width: '90%',
                                                                          maxHeight: '100vh',
                                                                          autoFocus: false,
                                                                          data: { bridgeInfo: bridge,
                                                                                  asGroup: this.groupInfo.id,
                                                                                },
                                                                        });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateBridges();
            }
        });
    }

    async updateCollaborators() {
        const collaborators = await this.groupService.getCollaboratorsOnGroup(this.groupInfo.id)

        collaborators.sort((a, b) => {
            // First try to sort by role
            if ((a.role === 'admin'  && b.role !== 'admin') ||
                (a.role === 'editor' && b.role === 'viewer')) {
                return -1;
            }

            if ((b.role === 'admin'  && a.role !== 'admin') ||
                (b.role === 'editor' && a.role === 'viewer')) {
                return 1;
            }

            // Else, sort alphabetically by username
            const nameA = a.username.toUpperCase();
            const nameB = b.username.toUpperCase();

            if (nameA < nameB) {
                return -1;
            }
            if (nameB < nameA) {
                return 1;
            }

            // Equal name and role
            return 0;
        });
        this.collaborators = collaborators;

        // Discover own user role
        for (let user of collaborators) {
            if (user.id == this.session.user_id) {
                if ((!this.userRole) || (user.role === 'admin')
                    || (user.role === 'editor' && this.userRole !== 'admin')) {

                    this.userRole = user.role;
                }
            }
        }
        this.canWriteToGroup = (this.userRole === 'admin') || (this.userRole === 'editor');
    }

    async openProgram(program: ProgramMetadata): Promise<void> {
        let programType = 'scratch';

        if (program.type === 'flow_program') {
            programType = 'flow';
        }

        this.router.navigateByUrl(`/programs/${program.id}/${programType}`);
    }

    async enableProgram(program: ProgramMetadata) {
        const session = await this.sessionService.getSession();
        await this.programService.setProgramStatus(JSON.stringify({"enable": true}),
                                                   program.id);
        program.enabled = true;
    }
}
