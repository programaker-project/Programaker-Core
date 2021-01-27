import { Component, Inject } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { EnvironmentService } from 'app/environment.service';
import { UserGroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { getGroupPictureUrl, getUserPictureUrl } from 'app/utils';
import { ProgramContent, ProgramMetadata } from '../../program';
import { getRequiredBridges, transformProgram } from 'app/program-transformations';
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
    links: {[key: string]: string} = {};
    selectedGroup: UserGroupInfo | null;
    programNameToSubmit: string;
    usedLinks: { from: BridgeConnection, to: BridgeIndexData }[] = [];

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
    cloningInProcess: boolean = false;
    cloningDone: boolean = false;
    createdProgramId: string;

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
            this.selectedGroup = null;
        }
        else {
            this.existingBridges = await this.bridgeService.listGroupBridges(this.destinationAccount);
            this.selectedGroup = this.user_groups.find(g => g.id === this.destinationAccount);
        }

        this.usedBridges = await this.getUsedBridges();

        for (const conn of this.usedBridges) {
            let linkedTo: string | null = null;

            const idIdx = this.existingBridges.findIndex((b) => b.id == conn.bridge_id );

            if (idIdx >= 0) {
                linkedTo = this.existingBridges[idIdx].id;
            }

            this.links[conn.bridge_id] = linkedTo;
        }

        this.loadingBridges = false;
        this.bridgesConnected = this.usedBridges.length === 0;

        this.onLinksUpdate();
    }

    onLinksUpdate() {
        this.bridgesConnected = !Object.keys(this.links).some(id => !this.links[id]);

        if (this.bridgesConnected) {
            const usedLinks = [];
            for (const srcBridge of this.usedBridges) {
                const toBridge = this.links[srcBridge.bridge_id];

                usedLinks.push({
                    from: srcBridge,
                    to: this.existingBridges.find(b => b.id === toBridge)
                })
            }
            this.usedLinks = usedLinks;
        }
    }

    prepareSummary() {
        this.programNameToSubmit = this.programNameFormGroup.get('programName').value;
    }

    async doClone() {
        this.cloningInProcess = true;

        transformProgram(this.data.program, this.links);

        let createdProgram: ProgramMetadata;
        if (this.destinationAccount === '__user') {
            createdProgram = await this.programService.createProgram(this.data.program.type, this.programNameToSubmit);
        }
        else {
            createdProgram = await this.programService.createProgramOnGroup(this.data.program.type, this.programNameToSubmit, this.destinationAccount);
        }

        const program = this.data.program;
        program.id = createdProgram.id;

        this.createdProgramId = program.id;

        await this.programService.updateProgramById(program);

        this.cloningDone = true;
        this.cloningInProcess = false;
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    onConfirm(): void {
        this.dialogRef.close({ success: true, program_id: this.createdProgramId });
    }
}
