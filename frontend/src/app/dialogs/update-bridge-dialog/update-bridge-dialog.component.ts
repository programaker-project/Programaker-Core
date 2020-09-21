import { Component, Inject } from '@angular/core';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeIndexData, BridgeSignal } from 'app/bridges/bridge';
import { BridgeService } from 'app/bridges/bridge.service';
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
    providers: [BridgeService, SessionService],
})
export class UpdateBridgeDialogComponent {
    session: Session;
    signalStream: Observable<BridgeSignal[]>;
    _stringify = JSON.stringify;

    constructor(public dialogRef: MatDialogRef<UpdateBridgeDialogComponent>,
                private bridgeService: BridgeService,
                private sessionService: SessionService,
                private dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: {
                    bridgeInfo: BridgeIndexData,
                    asGroup?: string,
                }) {

        this.sessionService.getSession().then(session => {
            this.session = session;
            if (this.session.tags.is_advanced) {
                const stream = this.bridgeService.getBridgeSignals(data.bridgeInfo.id, data.asGroup);

                this.signalStream = stream.pipe(
                    slidingWindow(10)
                );
            }
        });
    }

    async ngOnInit() {
    }

    onBack(): void {
        this.dialogRef.close({success: true});
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
