import { Component, Inject } from '@angular/core';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { AddConnectionDialogComponent } from 'app/connections/add-connection-dialog.component';
import { BridgeIndexData } from '../../bridges/bridge';
import { BridgeService } from '../../bridges/bridge.service';
import { ConnectionService } from '../../connection.service';
import { ServiceService } from '../../service.service';
import { SessionService } from '../../session.service';

@Component({
    selector: 'app-connect-to-available-dialog',
    templateUrl: 'connect-to-available-dialog.component.html',
    styleUrls: [
        'connect-to-available-dialog.component.scss',
        '../../libs/css/material-icons.css',
    ],
    providers: [BridgeService, SessionService, ServiceService, ConnectionService],
})
export class ConnectToAvailableDialogComponent {
    availableBridges: BridgeIndexData[] = null;

    constructor(public dialogRef: MatDialogRef<ConnectToAvailableDialogComponent>,
                public sessionService: SessionService,
                public serviceService: ServiceService,
                public connectionService: ConnectionService,
                public dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId?: string }) {
        if (!data) { data = this.data = {}; }


        let query: Promise<BridgeIndexData[]>;
        if (data.groupId) {
            query = this.connectionService.getAvailableBridgesForNewConnectionOnGroup(data.groupId);
        }
        else {
            query = this.connectionService.getAvailableBridgesForNewConnection();
        }

        query.then((bridges: BridgeIndexData[]) => {
            this.availableBridges = bridges.sort((a, b) => {
                return a.name.localeCompare(b.name, undefined, { ignorePunctuation: true, sensitivity: 'base' });
            });
        });
    }

    onNoClick(): void {
        this.dialogRef.close({success: false});
    }

    enableService(bridge: BridgeIndexData): void {
        const _dialogRef = this.dialog.open(AddConnectionDialogComponent, {
            data: { groupId: this.data.groupId, bridgeInfo: bridge }
        }).afterClosed().subscribe((result: {success:  boolean}) => {
            if (result && result.success) {
                this.dialogRef.close({success: true});
            }
        });
    }

}
