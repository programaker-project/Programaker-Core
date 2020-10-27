import { Component, Inject, ViewChild } from '@angular/core';
import { MatButton } from '@angular/material/button';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BridgeIndexData, BridgeResource, BridgeResourceEntry, BridgeSignal, BridgeTokenInfo, FullBridgeTokenInfo } from 'app/bridges/bridge';
import { BridgeService } from 'app/bridges/bridge.service';
import { UserGroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { Observable } from 'rxjs';
import { ConfirmDeleteDialogComponent } from '../confirm-delete-dialog/confirm-delete-dialog.component';
import { slidingWindow } from './sliding-window.operator';
import { Validators, FormBuilder, FormGroup } from '@angular/forms';
import { MatTooltip } from '@angular/material/tooltip';
import { MatSlideToggleChange } from '@angular/material/slide-toggle';
import { ConnectionService } from 'app/connection.service';
import { BridgeConnection } from 'app/connection';

const INCOMING_SIGNAL_STREAM_LEN = 100;

@Component({
    selector: 'app-update-bridge-dialog',
    templateUrl: 'update-bridge-dialog.component.html',
    styleUrls: [
        'update-bridge-dialog.component.css',
        '../../libs/css/material-icons.css',
    ],
    providers: [BridgeService, ConnectionService, GroupService, SessionService],
})
export class UpdateBridgeDialogComponent {
    session: Session;
    resources: BridgeResource[];
    adminGroups: UserGroupInfo[];
    groupsById: {[key: string]: UserGroupInfo[]};
    groups: UserGroupInfo[];
    expandedResources: {[key:string]: {[key: string]: boolean}} = {};
    dirtyShares = false;
    connectionUrl: string;

    expandedBridgeInfo = true;
    expandedResourceInfo = false;
    expandedTokens = false;
    tokens: (BridgeTokenInfo| FullBridgeTokenInfo)[];
    options: FormGroup;
    editableToken = false;
    saveTokenErrorMessage: string;

    _stringify = JSON.stringify;
    private groupsReady: Promise<void>;
    @ViewChild('applyShareChanges') applyShareChanges: MatButton;
    @ViewChild('resetShareChanges') resetShareChanges: MatButton;

    expandedSignalInfo = false;
    expandedIncomingSignals = false;
    expandedHistoricSignals = false;
    saveSignalsOnServer = false;
    saveSignals = false;
    signalStream: Observable<BridgeSignal[]>;
    connections: BridgeConnection[];

    @ViewChild('updateSaveSignalsButton') updateSaveSignalsButton: MatButton;
    lazyHistoricSignals = true;
    signalHistory: any[];

    constructor(public dialogRef: MatDialogRef<UpdateBridgeDialogComponent>,
                private bridgeService: BridgeService,
                private sessionService: SessionService,
                private groupService: GroupService,
                private connectionService: ConnectionService,
                private dialog: MatDialog,
                private notification: MatSnackBar,
                private formBuilder: FormBuilder,

                @Inject(MAT_DIALOG_DATA)
                public data: {
                    bridgeInfo: { id: string, name: string },
                    asGroup?: string,
                    isOwner: boolean,
                }) {

        this.options = this.formBuilder.group({
            newTokenName: ['', [Validators.required, Validators.minLength(4)]],
        });

        if (data.isOwner) {
            this.connectionUrl = bridgeService.getConnectionUrl(data.bridgeInfo.id);
            this.bridgeService.getBridgeTokens(data.bridgeInfo.id, data.asGroup).then(tokens => this.tokens = tokens);
        }

        this.sessionService.getSession().then(session => {
            this.session = session;

            this.updateConnections();
            this.resetShares();

            const stream = this.bridgeService.getBridgeSignals(data.bridgeInfo.id, data.asGroup);
            this.groupsReady = this.groupService.getUserGroups().then(groups => {
                const acceptedGroups: UserGroupInfo[] = [];
                for (const group of groups) {
                    if (group.role === 'admin') {
                        acceptedGroups.push(group);
                    }
                }

                const groupsById = {};
                for (const group of groups) {
                    groupsById[group.id] = group;
                }

                this.groups = groups;
                this.groupsById = groupsById;
                this.adminGroups = acceptedGroups;
            })

            this.signalStream = stream.pipe(
                slidingWindow(INCOMING_SIGNAL_STREAM_LEN),
            );
        });
    }

    async ngOnInit() {
    }

    onBack(): void {
        this.dialogRef.close({success: true});
    }

    toggleFold(resource: BridgeResource, entry: BridgeResourceEntry) {
        if (!this.expandedResources[resource.name]) {
            this.expandedResources[resource.name] = {};
        }

        this.expandedResources[resource.name][entry.id] = !this.expandedResources[resource.name][entry.id];
    }

    openFold(resource: BridgeResource, entry: BridgeResourceEntry) {
        if (!this.expandedResources[resource.name]) {
            this.expandedResources[resource.name] = {};
        }

        this.expandedResources[resource.name][entry.id] = true;
    }

    resetShares() {
        this.bridgeService.getBridgeResources(this.data.bridgeInfo.id, this.data.asGroup).then(resources => {
            this.resources = resources;
            this.dirtyShares = false;
        });
        this.expandedResources = {};
    }

    async addShare(resource: BridgeResource, entry: BridgeResourceEntry) {
        await this.groupsReady;

        if (this.adminGroups.length === 0) {
            this.notification.open('You need to be admin of a group to share a resource with it', 'ok', {
                duration: 5000
            });
        }
        if (!entry.shared_with) {
            entry.shared_with = [];
        }

        // Find the first group, which doesn't have this resource shared already
        const remainingGroups: UserGroupInfo[] = this.adminGroups.concat([]);
        while (remainingGroups.length > 0){
            const group = remainingGroups.shift();

            if (!entry.shared_with.find((share) => share.id === group.id)) {
                entry.shared_with.push({type: 'group', id: group.id});

                this.openFold(resource, entry);
                this.dirtyShares = true;

                return;
            }
        }

        if (remainingGroups.length === 0) {
            this.notification.open('You are already sharing this resource with all your groups.' , 'ok', {
                duration: 5000
            });
        }

    }

    removeShare(_resource: BridgeResource, entry: BridgeResourceEntry, index: number) {
        entry.shared_with.splice(index, 1);
        this.dirtyShares = true;
    }


    async applyShares() {
        // Set state to in-progress
        this.resetShareChanges.disabled = true;
        const buttonClassList = this.applyShareChanges._elementRef.nativeElement.classList;
        buttonClassList.add('started');
        buttonClassList.remove('completed');

        const operations : Promise<void>[] = [];
        for (const resource of this.resources) {
            const connections = {};
            for(const value of resource.values) {
                if (!connections[value.connection_id]) {
                    connections[value.connection_id] = {};
                }

                const val = {
                    name: value.name,
                    shared_with: value.shared_with,
                };

                connections[value.connection_id][value.id] = val;
            }

            for (const connectionId of Object.keys(connections)) {
                const op = this.bridgeService.setShares(connectionId, resource.name, connections[connectionId], { asGroup: this.data.asGroup });
                operations.push(op);
            }
        }

        try {
            await Promise.all(operations);
            this.dirtyShares = false;
        }
        finally {
            // Set state to "ready"
            buttonClassList.remove('started');
            buttonClassList.add('completed');
            this.resetShareChanges.disabled = false;
        }
    }

    deleteBridge() {
        const dialogRef = this.dialog.open(ConfirmDeleteDialogComponent, {
            data: this.data.bridgeInfo
        });

        dialogRef.afterClosed().subscribe(result => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const deletion = (this.bridgeService.deleteBridge(this.data.bridgeInfo.id)
                .catch(() => { return false; })
                .then(success => {
                    if (!success) {
                        return;
                    }

                    this.dialogRef.close({success: true});
                }));
        });
    }

    removeToken(token: BridgeTokenInfo) {
        const dialogRef = this.dialog.open(ConfirmDeleteDialogComponent, {
            data: token
        });

        dialogRef.afterClosed().subscribe(async (result) => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            await this.bridgeService.revokeToken(this.data.bridgeInfo.id, token.name, this.data.asGroup);

            const idx = this.tokens.indexOf(token);
            this.tokens.splice(idx, 1);
        });
    }

    createNewToken() {
        this.editableToken = true;
        this.expandedTokens = true;
    }

    async saveToken() {
        const tokenName = this.options.controls.newTokenName.value;

        let saved = false;
        let tokenInfo: BridgeTokenInfo;
        try {
            tokenInfo = await this.bridgeService.createBridgeToken(this.data.bridgeInfo.id, tokenName, this.data.asGroup);
            saved = true;
        }
        catch (err) {
            if ((err.name === 'HttpErrorResponse') && (err.status === 409)) {
                this.saveTokenErrorMessage = 'A token already exists with this name.';
                this.expandedTokens = true;
            }
            else {
                this.saveTokenErrorMessage = 'Error saving token. Try again later.';
            }
        }

        if (saved){
            this.options.controls.newTokenName.setValue('');
            this.editableToken = false;
            this.tokens.unshift(tokenInfo);

            this.expandedTokens = true;
        }
    }


    async updateConnections() {
        let connectionQuery;
        if (this.data.asGroup) {
            connectionQuery = this.connectionService.getConnectionsOnGroup(this.data.asGroup);
        }
        else {
            connectionQuery = this.connectionService.getConnections();
        }

        this.connections = (await connectionQuery).filter((c, _i, _a) => c.bridge_id === this.data.bridgeInfo.id);

        this.saveSignals = this.saveSignalsOnServer = this.connections.some((c) => c.saving);
    }

    onChangeSaveSignals(event: MatSlideToggleChange) {
        this.saveSignals = event.checked;
    }

    toggleExpandHistoricSignals() {
        this.expandedHistoricSignals = !this.expandedHistoricSignals;
        if (this.expandedHistoricSignals) {
            // Don't load historic signals until it's needed as it might contain a significant amount of data
            if (this.lazyHistoricSignals) {
                this.lazyHistoricSignals = false;
                this.bridgeService.getBridgeHistoric(this.data.bridgeInfo.id, this.data.asGroup).then(history => this.signalHistory = history)
            }
        }
    }

    async updateSaveSignals() {
        const buttonClass = this.updateSaveSignalsButton._elementRef.nativeElement.classList;
        buttonClass.add('started');
        buttonClass.remove('completed');
        this.saveSignalsOnServer = this.saveSignals;

        await this.bridgeService.setRecordBridgeConnections(this.data.bridgeInfo.id, this.saveSignalsOnServer, this.data.asGroup);

        buttonClass.remove('started');
        buttonClass.add('completed');
        setTimeout(() => buttonClass.remove('completed'), 1000);
    }
}
