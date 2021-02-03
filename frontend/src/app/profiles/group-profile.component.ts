import { Component, ViewChild, Input } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatTabGroup } from '@angular/material/tabs';
import { ActivatedRoute, Router } from '@angular/router';
import { BridgeIndexData, SharedResource } from 'app/bridges/bridge';
import { BrowserService } from 'app/browser.service';
import { IconReference } from 'app/connection';
import { ConnectionService } from 'app/connection.service';
import { EnvironmentService } from 'app/environment.service';
import { GroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Collaborator, CollaboratorRole, roleToIcon } from 'app/types/collaborator';
import { BridgeService } from '../bridges/bridge.service';
import { ProgramMetadata } from '../program';
import { ProgramService } from '../program.service';
import { Session } from '../session';
import { SessionService } from '../session.service';
import { getGroupPictureUrl, getUserPictureUrl, iconDataToUrl } from '../utils';
import { ProfileService, GroupProfileInfo } from './profile.service';


@Component({
    // moduleId: module.id,
    selector: 'app-my-group-profile',
    templateUrl: './group-profile.component.html',
    styleUrls: [
        'group-profile.component.scss',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class GroupProfileComponent {
    programs: ProgramMetadata[] = [];

    session: Session = null;
    @Input() profile: GroupProfileInfo;
    bridgeInfo: { [key:string]: { icon: string, name: string }} = {};
    collaborators: Collaborator[] = null;
    groupInfo: GroupInfo;

    readonly _iconDataToUrl: (icon: IconReference, bridge_id: string) => string;

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
        this._iconDataToUrl = iconDataToUrl.bind(this, environmentService);
    }

    ngOnInit(): void {
    }

    async openProgram(program: ProgramMetadata): Promise<void> {
        let programType = 'scratch';

        if (program.type === 'flow_program') {
            programType = 'flow';
        }

        this.router.navigateByUrl(`/programs/${program.id}/${programType}`);
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
