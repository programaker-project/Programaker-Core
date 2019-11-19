import { Component, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SessionService } from '../session.service';
import { ServiceService } from '../service.service';

@Component({
    selector: 'custom-signal-create-dialog-component',
    templateUrl: './create-dialog.component.html',
    styleUrls: [
        './create-dialog.component.css',
    ],
    providers: [SessionService, ServiceService],
})
export class CustomSignalCreateDialogComponent {
    signal: { name: string };

    constructor(
        public dialogRef: MatDialogRef<CustomSignalCreateDialogComponent>,
        public serviceService: ServiceService,
        @Inject(MAT_DIALOG_DATA)
        public data: { signal: { name: string } }
    ) {
        this.signal = data.signal;
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
