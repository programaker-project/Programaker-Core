import { Component, Inject } from '@angular/core';
import { ServiceEnableHowTo, ServiceEnableMessage, ServiceEnableEntry, ServiceEnableType } from '../service';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SessionService } from '../session.service';
import { ServiceService } from '../service.service';
import { BridgeIndexComponent } from './index.component';
import { BridgeIndexData } from './bridge';

@Component({
    selector: 'bridge-delete-dialog-component',
    templateUrl: './delete-dialog-component.html',
    styleUrls: [
        './delete-dialog-component.css',
    ],
    providers: [SessionService, ServiceService],
})

export class BridgeDeleteDialogComponent {
    bridge: BridgeIndexData;

    constructor(
        public dialogRef: MatDialogRef<BridgeDeleteDialogComponent>,
        public serviceService: ServiceService,
        @Inject(MAT_DIALOG_DATA)
        public data: { bridge: BridgeIndexData }
    ) {
        this.bridge = data.bridge;
    }


    onNoClick(): void {
        this.dialogRef.close();
    }
}
