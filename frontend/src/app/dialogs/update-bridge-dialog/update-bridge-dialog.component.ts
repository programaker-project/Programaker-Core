import { Component, Inject, ViewChild } from '@angular/core';
import { MatButton } from '@angular/material/button';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatSnackBar } from '@angular/material/snack-bar';
import { BridgeIndexData, BridgeResource, BridgeResourceEntry, BridgeSignal } from 'app/bridges/bridge';
import { BridgeService } from 'app/bridges/bridge.service';
import { UserGroupInfo } from 'app/group';
import { GroupService } from 'app/group.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { Observable } from 'rxjs';
import { ConfirmDeleteDialogComponent } from '../confirm-delete-dialog/confirm-delete-dialog.component';
import { slidingWindow } from './sliding-window.operator';

@Component({
    selector: 'app-update-bridge-dialog',
    templateUrl: 'update-bridge-dialog.component.html',
    styleUrls: [
        'update-bridge-dialog.component.css',
        '../../libs/css/material-icons.css',
    ],
    providers: [BridgeService, GroupService, SessionService],
})
export class UpdateBridgeDialogComponent {
    session: Session;
    signalStream: Observable<BridgeSignal[]>;
    resources: BridgeResource[];
    adminGroups: UserGroupInfo[];
    groupsById: {[key: string]: UserGroupInfo[]};
    groups: UserGroupInfo[];
    expandedResources: {[key:string]: {[key: string]: boolean}} = {};
    dirtyShares = false;

    _stringify = JSON.stringify;
    private groupsReady: Promise<void>;
    @ViewChild('applyShareChanges') applyShareChanges: MatButton;
    @ViewChild('resetShareChanges') resetShareChanges: MatButton;

    constructor(public dialogRef: MatDialogRef<UpdateBridgeDialogComponent>,
                private bridgeService: BridgeService,
                private sessionService: SessionService,
                private groupService: GroupService,
                private dialog: MatDialog,
                private notification: MatSnackBar,

                @Inject(MAT_DIALOG_DATA)
                public data: {
                    bridgeInfo: BridgeIndexData,
                    asGroup?: string,
                }) {

        this.sessionService.getSession().then(session => {
            this.session = session;

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
                slidingWindow(10)
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

                connections[value.connection_id][value.id] = value.shared_with;
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
}
