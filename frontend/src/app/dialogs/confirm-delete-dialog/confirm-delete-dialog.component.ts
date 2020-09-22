import { Component, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'app-confirm-delete-dialog',
    templateUrl: 'confirm-delete-dialog.html',
    styleUrls: [
        'confirm-delete-dialog.css',
    ]
})

export class ConfirmDeleteDialogComponent {
    constructor(public dialogRef: MatDialogRef<ConfirmDeleteDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: { name: string }) {
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
