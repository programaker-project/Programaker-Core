import { Component, Inject } from '@angular/core';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeIndexData, BridgeResource, BridgeResourceEntry, BridgeSignal } from 'app/bridges/bridge';
import { BridgeService } from 'app/bridges/bridge.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { Observable } from 'rxjs';
import { ConfirmDeleteDialogComponent } from '../confirm-delete-dialog/confirm-delete-dialog.component';
import { slidingWindow } from './sliding-window.operator';
import { GroupService } from 'app/group.service';
import { UserGroupInfo } from 'app/group';
import { MatSnackBar } from '@angular/material/snack-bar';

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

    _stringify = JSON.stringify;
    private groupsReady: Promise<void>;

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

            this.bridgeService.getBridgeResources(data.bridgeInfo.id, data.asGroup).then(resources => this.resources = resources);
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

    async openShare(resource: BridgeResource, entry: BridgeResourceEntry) {
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
