import { Component, Inject } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { EnvironmentService } from 'app/environment.service';
import { UserGroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { getGroupPictureUrl, getUserPictureUrl } from 'app/utils';
import { ProgramContent } from '../../program';
import { getRequiredBridges } from 'app/program-transformations';
import { ProgramService } from 'app/program.service';
import { BridgeService } from 'app/bridges/bridge.service';
import { ConnectionService } from 'app/connection.service';
import { BridgeConnection } from 'app/connection';
import { BridgeIndexData } from 'app/bridges/bridge';

export type CloneProgramDialogComponentData = {
    name: string,
    program: ProgramContent,
}

@Component({
    selector: 'app-clone-program-dialog',
    templateUrl: 'clone-program-dialog.html',
    styleUrls: [
        'clone-program-dialog.scss',
        '../../libs/css/material-icons.css',
    ]
})
export class CloneProgramDialogComponent {

    // Models
    destinationAccount: '__user' | string | null;
    programNameFormGroup: FormGroup;
    bridgesConnected = false;
    loadingBridges = true;
    links: {[key: string]: BridgeIndexData[]} = {};

    // Utils used on template
    readonly _getUserPicture: (userId: string) => string;
    readonly _getGroupPicture: (groupId: string) => string;

    // Information
    session: Session;
    user_groups: UserGroupInfo[];
    programBridges: string[];
    connectionQuery: Promise<BridgeConnection[]>;
    usedBridges: BridgeConnection[];
    existingBridges: BridgeIndexData[];

    constructor(public dialogRef: MatDialogRef<CloneProgramDialogComponent>,
                private environmentService: EnvironmentService,
                public sessionService: SessionService,
                private groupService: GroupService,
                private formBuilder: FormBuilder,
                private programService: ProgramService,
                private bridgeService: BridgeService,
                private connectionService: ConnectionService,

                @Inject(MAT_DIALOG_DATA)
                public data: CloneProgramDialogComponentData) {

        this._getUserPicture = getUserPictureUrl.bind(this, environmentService);
        this._getGroupPicture = getGroupPictureUrl.bind(this, environmentService);

        this.programNameFormGroup = this.formBuilder.group({
            programName: [data.name, [Validators.required, Validators.minLength(4)]],
        });


        sessionService.getSession().then(session => this.session = session );

        groupService.getUserGroups()
            .then(groups => this.user_groups = groups);

        this.programBridges = getRequiredBridges(data.program);
        this.connectionQuery = connectionService.getConnectionsOnProgram(data.program.id);
    }

    async getUsedBridges(): Promise<BridgeConnection[]> {
        const connections = await this.connectionQuery;
        const usedOnProgram = [];

        for (const conn of connections) {
            if (this.programBridges.indexOf(conn.bridge_id) >= 0) {
                usedOnProgram.push(conn);
            }
        }

        return usedOnProgram;
    }

    async prepareBridges() {
        this.loadingBridges = true;
        this.bridgesConnected = false;

        if (this.destinationAccount === '__user') {
            this.existingBridges = (await this.bridgeService.listUserBridges()).bridges;
        }
        else {
            this.existingBridges = await this.bridgeService.listGroupBridges(this.destinationAccount);
        }

        this.usedBridges = await this.getUsedBridges();

        for (const conn of this.usedBridges) {
            let linkedTo = null;

            const idIdx = this.existingBridges.findIndex((b) => b.id == conn.bridge_id );

            if (idIdx >= 0) {
                linkedTo = this.existingBridges[idIdx];
            }

            this.links[conn.bridge_id] = linkedTo.id;
        }

        this.loadingBridges = false;
        this.bridgesConnected = this.usedBridges.length === 0;

        this.onLinksUpdate();
    }

    onLinksUpdate() {
        this.bridgesConnected = !Object.keys(this.links).some(id => !this.links[id]);
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    onConfirm(): void {
    }
}
