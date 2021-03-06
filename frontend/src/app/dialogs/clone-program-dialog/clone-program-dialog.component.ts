import { Component, Inject, ViewChild } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatStepper } from '@angular/material/stepper';
import { AssetService } from 'app/asset.service';
import { SharedResource } from 'app/bridges/bridge';
import { BridgeConnection } from 'app/connection';
import { ConnectionService } from 'app/connection.service';
import { EnvironmentService } from 'app/environment.service';
import { UserGroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { getRequiredAssets, getRequiredBridges, transformProgram } from 'app/program-transformations';
import { ProgramService } from 'app/program.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { getGroupPictureUrl, getUserPictureUrl } from 'app/utils';
import { ProgramContent, ProgramMetadata } from '../../program';

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
    cloneToFormGroup: FormGroup;
    bridgeConnectionFormGroup: FormGroup;
    bridgesConnected = false;
    loadingBridges = true;
    links: {[key: string]: string} = {};
    selectedGroup: UserGroupInfo | null;
    programNameToSubmit: string;
    usedLinks: { from: BridgeConnection, to: BridgeConnection }[] = [];

    // Utils used on template
    readonly _getUserPicture: (userId: string) => string;
    readonly _getGroupPicture: (groupId: string) => string;

    // Information
    readonly sourceProgramId: string;
    session: Session;
    user_groups: UserGroupInfo[];
    programBridges: string[];
    connectionQuery: Promise<BridgeConnection[]>;
    usedBridges: BridgeConnection[];
    existingBridges: BridgeConnection[];
    cloningInProcess: boolean = false;
    cloningDone: boolean = false;
    createdProgramId: string;

    // Views
    @ViewChild('stepper') stepper: MatStepper;
    sharedResourcesQuery: Promise<SharedResource[]>;

    constructor(private dialogRef: MatDialogRef<CloneProgramDialogComponent>,
                environmentService: EnvironmentService,
                sessionService: SessionService,
                private groupService: GroupService,
                private formBuilder: FormBuilder,
                private programService: ProgramService,
                private connectionService: ConnectionService,
                private assetService: AssetService,

                @Inject(MAT_DIALOG_DATA)
                public data: CloneProgramDialogComponentData) {

        this.sourceProgramId = data.program.id;

        this._getUserPicture = getUserPictureUrl.bind(this, environmentService);
        this._getGroupPicture = getGroupPictureUrl.bind(this, environmentService);

        this.cloneToFormGroup = this.formBuilder.group({
            cloneTo: new FormControl('', (control) => {
                if (this.destinationAccount) {
                    return null;
                }
                else {
                    return { error: 'Not destination account selected' };
                }
            }),
        });
        this.bridgeConnectionFormGroup = this.formBuilder.group({
            bridgeConnections: new FormControl('', (control) => {
                if (this.bridgesConnected) {
                    return null;
                }
                else {
                    return { error: 'Not all bridges are connected' };
                }
            }),
        });

        this.programNameFormGroup = this.formBuilder.group({
            programName: [data.name, [Validators.required, Validators.minLength(4)]],
        });


        sessionService.getSession().then(session => this.session = session );

        this.groupService.getUserGroups()
            .then(groups => this.user_groups = groups);

        this.programBridges = getRequiredBridges(data.program);
        this.connectionQuery = this.connectionService.getConnectionsOnProgram(data.program.id);
        this.sharedResourcesQuery = this.programService.getProgramSharedResources(data.program.id);
    }

    updateDestinationAccount(value: string) {
        this.destinationAccount = value;
        this.cloneToFormGroup.get('cloneTo').updateValueAndValidity();
    }

    async getUsedBridges(): Promise<BridgeConnection[]> {
        const connections = await this.connectionQuery;
        const sharedResources = await this.sharedResourcesQuery;
        const usedOnProgram: BridgeConnection[] = [];

        for (const conn of connections) {
            if (this.programBridges.indexOf(conn.bridge_id) >= 0) {
                usedOnProgram.push(conn);
            }
        }
        for (const res of sharedResources) {
            if (this.programBridges.indexOf(res.bridge_id) >= 0) {
                usedOnProgram.push({
                    connection_id: null,
                    name: res.name,
                    icon: res.icon,
                    bridge_id: res.bridge_id,
                    bridge_name: res.name,
                    saving: null,
                });
            }
        }

        return usedOnProgram;
    }

    async prepareBridges() {
        this.loadingBridges = true;
        this.bridgesConnected = false;

        if (this.destinationAccount === '__user') {
            this.existingBridges = (await this.connectionService.getConnections());
            this.selectedGroup = null;
        }
        else {
            this.existingBridges = await this.connectionService.getConnectionsOnGroup(this.destinationAccount);
            this.selectedGroup = this.user_groups.find(g => g.id === this.destinationAccount);

            const sharedBridgesOnTarget = await this.groupService.getSharedResources(this.destinationAccount);
            for (const share of sharedBridgesOnTarget) {
                this.existingBridges.push({
                    connection_id: null,
                    name: share.name,
                    icon: share.icon,
                    bridge_id: share.bridge_id,
                    bridge_name: share.name,
                    saving: null,
                });
            }
        }

        this.usedBridges = await this.getUsedBridges();

        for (const conn of this.usedBridges) {
            let linkedTo: string | null = null;

            const idIdx = this.existingBridges.findIndex((b) => b.bridge_id == conn.bridge_id );

            if (idIdx >= 0) {
                linkedTo = this.existingBridges[idIdx].bridge_id;
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
                    to: this.existingBridges.find(b => b.bridge_id === toBridge)
                })
            }
            this.usedLinks = usedLinks;
        }

        this.bridgeConnectionFormGroup.get('bridgeConnections').updateValueAndValidity();
    }

    prepareSummary() {
        this.programNameToSubmit = this.programNameFormGroup.get('programName').value;
    }

    async doClone() {
        // Lock other steps
        this.stepper.steps.forEach(step => step.editable = false);

        // Start cloning
        this.cloningInProcess = true;

        const assets = getRequiredAssets(this.data.program);

        // Change program to fit the new user
        transformProgram(this.data.program, this.links);

        // Create the program
        let createdProgram: ProgramMetadata;
        if (this.destinationAccount === '__user') {
            createdProgram = await this.programService.createProgram(this.data.program.type, this.programNameToSubmit);
        }
        else {
            createdProgram = await this.programService.createProgramOnGroup(this.data.program.type, this.programNameToSubmit, this.destinationAccount);
        }

        // Upload the program itself
        const program = this.data.program;
        program.id = createdProgram.id;

        this.createdProgramId = program.id;

        await this.programService.updateProgramById(program);

        // Upload the required assets
        const copies = assets.map(assetId =>
            this.assetService.copyAssetToProgram(this.sourceProgramId, assetId, this.createdProgramId)
        );

        await Promise.all(copies);

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
