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

type TutorialData = { description: string, icons: string[], url: string };

@Component({
    // moduleId: module.id,
    selector: 'app-my-dashboard',
    templateUrl: './dashboard.component.html',
    providers: [BridgeService, GroupService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'dashboard.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class DashboardComponent {
    programs: ProgramMetadata[] = [];
    session: Session = null;
    profile: {type: 'user' | 'group', name: string, groups: GroupInfo[], picture: string};
    bridgeInfo: { [key:string]: { icon: string, name: string }} = {};
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
        'programs',
        'archived-programs',
        'bridges',
        'info',
    ];
    groupInfo: GroupInfo;
    userRole: CollaboratorRole | null;
    canWriteToGroup: boolean;

    constructor(
        private browser: BrowserService,
        private programService: ProgramService,
        private sessionService: SessionService,
        private groupService: GroupService,
        private router: Router,
        private route: ActivatedRoute,

        private dialog: MatDialog,
        private bridgeService: BridgeService,
    ) {
    }

    ngOnInit(): void {
        this.sessionService.getSession()
            .then(async (session) => {
                this.session = session;

                if (!session.active) {
                    this.router.navigate(['/login'], {replaceUrl:true});
                } else {

                    const params = this.route.params['value'];
                    if (params.group_name !== undefined) {
                        // Group Dashboard
                        const groupName = params.group_name;

                        this.profile = {
                            name: groupName,
                            'type': 'group',
                            groups: null,
                            picture: null,
                        };

                        this.groupInfo = await this.groupService.getGroupWithName(groupName);
                        this.profile.picture = getGroupPictureUrl(this.groupInfo.id);

                        this.updateCollaborators();
                        this.updateSharedResources();
                    }
                    else {
                        this.profile = {
                            name: session.username,
                            'type': 'user',
                            groups: null,
                            picture: getUserPictureUrl(session.user_id)
                        };

                        this.groupService.getUserGroups()
                            .then(groups => this.profile.groups = groups);

                    }

                    this.updatePrograms();
                    this.updateBridges();
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    ngAfterViewInit() {
        let unsubscribe = false;
        let subscription = null;
        // The same behavior might be achieved with .toPromise(), but it
        // seems to have problems (with race conditions?).
        subscription = this.route.fragment.subscribe({
            next: (fragment => {
                const idx = this.tabFragName.indexOf(fragment);
                if (idx >= 0) {
                    this.navTabGroup.selectedIndex = idx;
                }

                if (subscription !== null) {
                    subscription.unsubscribe();
                }
                else {
                    // In case the subscription assignation has not happened yet, take not of it to
                    // unsubscribe as soon as possible.
                    unsubscribe = true;
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

                history.replaceState(currState, '', this.updateAnchor(this.browser.window.location.href, this.tabFragName[idx]));
                this.programSettingsOpened = {};
            }
        });
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
        if (this.session.tags.is_in_preview) {
            const dialogRef = this.dialog.open(SelectProgrammingModelDialogComponent, { width: '90%' });

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
        else {
            let programCreation: Promise<ProgramMetadata>;
            if (this.groupInfo) {
                programCreation = this.programService.createProgramOnGroup('scratch_program', null, this.groupInfo.id);
            }
            else {
                programCreation = this.programService.createProgram('scratch_program');
            }
            programCreation.then(program => this.openProgram(program));
        }
    }

    async updatePrograms() {
        let programListing: Promise<ProgramMetadata[]>;
        if (this.groupInfo) {
            programListing = this.programService.getProgramsOnGroup(this.groupInfo.id);
        }
        else {
            programListing = this.programService.getPrograms();
        }

        const programs = await programListing;
        programs.sort((a, b) => {
            if (a.name < b.name) {
                return -1;
            }
            if (a.name > b.name) {
                return 1;
            }
            return 0;
        });
        this.programs = programs;
        this.programSettingsOpened = {};
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
            if (a.name < b.name) {
                return -1;
            }
            if (a.name > b.name) {
                return 1;
            }
            return 0;
        });

        for (const bridge of this.bridges) {
            this.bridgeInfo[bridge.id] = { name: bridge.name, icon: iconDataToUrl(bridge.icon, bridge.id) };
        }
    }


    async updateSharedResources() {
        if (!this.groupInfo) {
            return;
        }

        this.sharedResources = await this.groupService.getSharedResources(this.groupInfo.id);

        for (const conn of this.sharedResources){
            this.bridgeInfo[conn.bridge_id] = {
                icon: iconDataToUrl(conn.icon, conn.bridge_id),
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

    openBridgePanel(bridge: {id: string, name: string} ) {
        const dialogRef = this.dialog.open(UpdateBridgeDialogComponent, { width: '90%',
                                                                          maxHeight: '100vh',
                                                                          maxWidth: '100vw',
                                                                          autoFocus: false,
                                                                          data: {
                                                                              bridgeInfo: bridge,
                                                                              asGroup: this.groupInfo?.id,
                                                                          },
                                                                        });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.updateBridges();
            }
        });
    }


    async openProgram(program: ProgramMetadata): Promise<void> {
        let programType = 'scratch';

        if (program.type === 'flow_program') {
            programType = 'flow';
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
    readonly _getUserPicture = getUserPictureUrl;
    readonly _iconDataToUrl = iconDataToUrl;
    readonly _roleToIcon = roleToIcon;

    _toCapitalCase(x: string): string {
        if (!x || x.length == 0) {
            return x;
        }
        return x[0].toUpperCase() + x.substr(1);
    }
}
