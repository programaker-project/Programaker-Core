import { Component, ViewChild } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatTabGroup } from '@angular/material/tabs';
import { ActivatedRoute, Router } from '@angular/router';
import { BridgeIndexData, SharedResource } from 'app/bridges/bridge';
import { BrowserService } from 'app/browser.service';
import { AddBridgeDialogComponent } from 'app/dialogs/add-bridge-dialog/add-bridge-dialog.component';
import { EditCollaboratorsDialogComponent } from 'app/dialogs/editor-collaborators-dialog/edit-collaborators-dialog.component';
import { UpdateBridgeDialogComponent } from 'app/dialogs/update-bridge-dialog/update-bridge-dialog.component';
import { GroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Collaborator, CollaboratorRole, roleToIcon } from 'app/types/collaborator';
import { BridgeService } from '../bridges/bridge.service';
import { MonitorService } from '../monitor.service';
import { ProgramMetadata, ProgramType } from '../program';
import { ProgramService } from '../program.service';
import { SelectProgrammingModelDialogComponent } from '../programs/select-programming-model-dialog/select-programming-model-dialog.component';
import { ServiceService } from '../service.service';
import { Session } from '../session';
import { SessionService } from '../session.service';
import { getGroupPictureUrl, getUserPictureUrl, iconDataToUrl } from '../utils';
import { ConnectionService } from 'app/connection.service';
import { BridgeConnectionWithIconUrl, IconReference } from 'app/connection';
import { EnvironmentService } from 'app/environment.service';
import { ProfileService, GroupProfileInfo } from 'app/profiles/profile.service';
import { Subscription } from 'rxjs';
import { AddConnectionDialogComponent } from 'app/connections/add-connection-dialog.component';
import { ConnectToAvailableDialogComponent } from 'app/dialogs/connect-to-available-dialog/connect-to-available-dialog.component';

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
export class DashboardComponent {
    programs: ProgramMetadata[] = [];
    connections: BridgeConnectionWithIconUrl[] = null;

    session: Session = null;
    profile: {type: 'user' | 'group', name: string, groups: GroupInfo[], picture: string};
    bridgeInfo: { [key:string]: { icon: string, name: string }} = {};
    bridgesById: { [key:string]: BridgeIndexData} = {};
    collaborators: Collaborator[] = null;

    bridges: BridgeIndexData[] = null;
    tutorials: TutorialData[] = [
        {
            description: "Create a weather chatbot",
            icons: [ "/assets/icons/telegram_logo.png", "/assets/icons/aemet_logo.png" ],
            url: "https://docs.programaker.com/tutorials/weather-bot.html",
        },
    ];
    programSettingsOpened: { [key: string]: false | 'archive' } = {};

    sharedResources: SharedResource[];

    @ViewChild('navTabGroup') navTabGroup: MatTabGroup;

    tabFragName = [
        'profile',
        'programs',
        'archived-programs',
        'bridges',
        'info',
    ];
    groupInfo: GroupInfo;
    userRole: CollaboratorRole | null;
    groupProfile: GroupProfileInfo;

    canWriteToGroup: boolean;
    bridgesQuery: Promise<void>;

    readonly _getUserPicture: (userId: string) => string;
    readonly _iconDataToUrl: (icon: IconReference, bridge_id: string) => string;
    isOwnUser: boolean;
    private _isReadyForLoadingTabs: boolean = true;
    private _moveToTab: () => void;

    constructor(
        private browser: BrowserService,
        private programService: ProgramService,
        private sessionService: SessionService,
        private groupService: GroupService,
        private connectionService: ConnectionService,
        private router: Router,
        private route: ActivatedRoute,
        private environmentService: EnvironmentService,
        private profileService: ProfileService,

        private dialog: MatDialog,
        private bridgeService: BridgeService,
    ) {
        this._getUserPicture = getUserPictureUrl.bind(this, environmentService);
        this._iconDataToUrl = iconDataToUrl.bind(this, environmentService);

        this.route.data
            .subscribe((data: { programs: ProgramMetadata[] }) => {
                this.programs = data.programs?.sort((a, b) => {
                    return a.name.localeCompare(b.name, undefined, { ignorePunctuation: true, sensitivity: 'base' });
                });
            });
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(async (session) => {
                this.session = session;
                const params = (this.route.params as any)['value'];
                if (params.group_name !== undefined) {
                    this.isOwnUser = false;

                    // Group Dashboard
                    const groupName = params.group_name;

                    this.profile = {
                        name: groupName,
                        'type': 'group',
                        groups: null,
                        picture: null,
                    };

                    this.groupProfile = await this.profileService.getProfileFromGroupname(groupName);

                    this.groupService.getGroupWithName(groupName).then(groupInfo => {
                        this.groupInfo = groupInfo;
                        this.profile.picture = getGroupPictureUrl(this.environmentService, this.groupInfo.id);

                        this.bridgesQuery = this.updateBridges();
                        this.updateConnections();

                        this.updateSharedResources();
                        return this.updateCollaborators();
                    })
                        .catch(err => console.error(err))
                        .then(() => this._tabReady())
                }
                else {
                    if (!session.active) {
                        this.router.navigate(['/login'], {replaceUrl:true});
                    } else {
                        this.isOwnUser = true;
                        this._tabReady();

                        this.profile = {
                            name: session.username,
                            'type': 'user',
                            groups: null,
                            picture: getUserPictureUrl(this.environmentService, session.user_id)
                        };

                        this.groupService.getUserGroups()
                            .then(groups => this.profile.groups = groups);
                    }

                    this.bridgesQuery = this.updateBridges();
                    this.updateConnections();
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    _tabReady() {
        // This activates `_moveToTab` when all the data necessary to know which
        // tabs are to be shown is available. In case `_moveToTab` is not
        // available yet, it records that the necessary state has been reached
        // so the function that defines `_moveToTab` can call it directly.

        this._isReadyForLoadingTabs = true;
        if (this._moveToTab) {
            this._moveToTab();
        }
    }

    ngAfterViewInit() {
        let unsubscribe = false;
        let subscription: Subscription = null;
        // The same behavior might be achieved with .toPromise(), but it
        // seems to have problems (with race conditions?).
        subscription = this.route.fragment.subscribe({
            next: (fragment => {
                this._moveToTab = (() => {
                    const idx = this.tabFragName.indexOf(fragment) + (this._isProfileTabPresent() ? 0 : - 1);
                    if (idx >= 0) {
                        this.navTabGroup.selectedIndex = idx;
                    }

                    if (subscription !== null) {
                        subscription.unsubscribe();
                    }
                    else {
                        // In case the subscription assignation has not happened yet, take note of it to
                        // unsubscribe as soon as possible.
                        unsubscribe = true;
                    }
                });
                if (this._isReadyForLoadingTabs) {
                    this._moveToTab();
                }
            })
        });
        if (unsubscribe) {
            // The first value has read before the `subcription` variable has been assigned.
            // Now the only thing that remains is to perform the unsubscription.
            subscription.unsubscribe();
        }

        this.navTabGroup.selectedIndexChange.subscribe({
            next: (idx: number) => {
                const currState = history.state;

                history.replaceState(currState, '', this.updateAnchor(this.browser.window.location.href, this.getTabFragName(idx)));
                this.programSettingsOpened = {};
            }
        });
    }

    private getTabFragName(idx: number) {
        return this.tabFragName[this._isProfileTabPresent() ? idx : idx + 1];
    }

    _isProfileTabPresent() {
        return this.profile && this.profile.type === 'group' && this.groupProfile;
    }

    private updateAnchor(href: string, anchor: string): string {
        const anchorStart = href.indexOf('#');
        if (anchorStart < 0) {
            return href + '#' + anchor;
        }
        else {
            return href.substring(0, anchorStart) + '#' + anchor;
        }
    }

    addProgram(): void {
        const dialogRef = this.dialog.open(SelectProgrammingModelDialogComponent, { width: '90%', data: {
            is_advanced_user: this.session.tags.is_advanced,
            is_user_in_preview: this.session.tags.is_in_preview,
        }});

        dialogRef.afterClosed().subscribe((result: {success: boolean, program_type: ProgramType, program_name: string}) => {
            if (result && result.success) {
                let programCreation: Promise<ProgramMetadata>;
                if (this.groupInfo) {
                    programCreation = this.programService.createProgramOnGroup(result.program_type, result.program_name, this.groupInfo.id);
                }
                else {
                    programCreation = this.programService.createProgram(result.program_type, result.program_name);
                }

                programCreation.then(program => this.openProgram(program));
            }
        });
    }

    addConnection(): void {
        const dialogRef = this.dialog.open(ConnectToAvailableDialogComponent, { width: '90%',
                                                                                data: { groupId: this.groupInfo?.id }});

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateConnections();
            }
        });
    }

    addBridge(): void {
        const dialogRef = this.dialog.open(AddBridgeDialogComponent, { width: '80%',
                                                                       data: { groupId: this.groupInfo?.id },
                                                                     });

        dialogRef.afterClosed().subscribe((result: {success: boolean, bridgeId?: string, bridgeName?: string}) => {
            if (result && result.success) {
                this.updateBridges();

                this.openBridgePanel({ id: result.bridgeId, name: result.bridgeName });
            }
        });
    }

    async updateBridges() {
        if (this.groupInfo) {
            this.bridges = await this.bridgeService.listGroupBridges(this.groupInfo.id);
        }
        else {
            this.bridges = (await this.bridgeService.listUserBridges()).bridges;
        }
        this.bridges.sort((a, b) => {
            return a.name.localeCompare(b.name, undefined, { ignorePunctuation: true, sensitivity: 'base' });
        });

        for (const bridge of this.bridges) {
            this.bridgeInfo[bridge.id] = {
                name: bridge.name,
                icon: iconDataToUrl(this.environmentService, bridge.icon, bridge.id)
            };
            this.bridgesById[bridge.id] = bridge;
        }
    }


    async updateSharedResources() {
        if (!this.groupInfo) {
            return;
        }

        this.sharedResources = await this.groupService.getSharedResources(this.groupInfo.id);

        for (const conn of this.sharedResources){
            this.bridgeInfo[conn.bridge_id] = {
                icon: iconDataToUrl(this.environmentService, conn.icon, conn.bridge_id),
                name: conn.name
            };
        }
    }

    addCollaborators(): void {
        const dialogRef = this.dialog.open(EditCollaboratorsDialogComponent, { width: '90%', maxHeight: '100vh', maxWidth: '100vw',
                                                                               data: { groupId: this.groupInfo.id,
                                                                                       existingCollaborators: this.collaborators,
                                                                                     },
                                                                             });

        dialogRef.afterClosed().subscribe(async (result: {success: boolean}) => {
            if (result && result.success) {
                this.updateCollaborators();
            }
        });
    }

    async updateCollaborators() {
        if (!this.groupInfo) {
            return;
        }

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
            return a.username.localeCompare(b.username, undefined, { ignorePunctuation: true, sensitivity: 'base' });
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

    async updateConnections() {
        let connectionQuery;
        if (this.groupInfo) {
            connectionQuery = this.connectionService.getConnectionsOnGroup(this.groupInfo.id);
        }
        else {
            connectionQuery = this.connectionService.getConnections();
        }

        const connections = await connectionQuery;
        this.connections = connections.map((v, _i, _a) => {
            const icon_url = iconDataToUrl(this.environmentService, v.icon, v.bridge_id);

            return { conn: v, extra: {icon_url: icon_url }};
        }).sort((a, b) => {
            return a.conn.bridge_name.localeCompare(b.conn.bridge_name, undefined, { ignorePunctuation: true, sensitivity: 'base' });
        });

        await this.bridgesQuery; // Wait for the bridges query to complete
    }

    openBridgePanel(bridge: {id: string, name: string}, isOwner: boolean=true ) {
        const dialogRef = this.dialog.open(UpdateBridgeDialogComponent, { width: '90%',
                                                                          maxHeight: '100vh',
                                                                          maxWidth: '100vw',
                                                                          autoFocus: false,
                                                                          data: {
                                                                              bridgeInfo: bridge,
                                                                              asGroup: this.groupInfo?.id,
                                                                              isOwner: isOwner,
                                                                          },
                                                                        });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateBridges();
            }
        });
    }

    openBridgePanelFromConnection(connection: BridgeConnectionWithIconUrl) {
        return this.openBridgePanel({
            id: connection.conn.bridge_id,
            name: connection.conn.bridge_name,
        }, false);
    }


    async openProgram(program: ProgramMetadata): Promise<void> {
        let programType = 'scratch';

        if (program.type === 'flow_program') {
            programType = 'flow';
        }
        if (program.type === 'spreadsheet_program') {
            programType = 'spreadsheet';
        }

        this.router.navigateByUrl(`/programs/${program.id}/${programType}`);
    }

    async enableProgram(program: ProgramMetadata) {
        await this.programService.setProgramStatus(JSON.stringify({"enable": true}),
                                                   program.id);
        program.enabled = true;
    }

    async archiveProgram(program: ProgramMetadata) {
        await this.programService.setProgramStatus(JSON.stringify({"enable": false}),
                                                   program.id);
        program.enabled = false;
        delete this.programSettingsOpened[program.id];
    }

    async toggleShowProgramArchive(program: ProgramMetadata) {
        if (this.programSettingsOpened[program.id] === 'archive') {
            this.programSettingsOpened[program.id] = false;
        }
        else {
            this.programSettingsOpened[program.id] = 'archive';
        }
    }

    getEnabled(programs: ProgramMetadata[]): ProgramMetadata[] {
        return programs.filter((p) => p.enabled);
    }

    getArchived(programs: ProgramMetadata[]): ProgramMetadata[] {
        return programs.filter((p) => !p.enabled);
    }

    createGroup() {
        this.router.navigate(['/new/group']);
    }

    openTutorial(tutorial: TutorialData) {
        const win = this.browser.window.open(tutorial.url, '_blank');
        win.focus();
    }

    openGroup(group: GroupInfo) {
        this.router.navigateByUrl(`/groups/${group.canonical_name}`);
    }

    // Utils
    readonly _roleToIcon = roleToIcon;

    _toCapitalCase(x: string): string {
        if (!x || x.length == 0) {
            return x;
        }
        return x[0].toUpperCase() + x.substr(1);
    }
}
