import { Component, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { VisibilityEnum } from 'app/program';

@Component({
    selector: 'app-change-program-visibility-dialog',
    templateUrl: 'change-program-visibility-dialog.html',
    styleUrls: [
        'change-program-visibility-dialog.scss',
        '../../libs/css/material-icons.css',
    ]
})
export class ChangeProgramVisilibityDialog {
    visibility: VisibilityEnum;

    constructor(public dialogRef: MatDialogRef<ChangeProgramVisilibityDialog>,
                @Inject(MAT_DIALOG_DATA)
                public data: { name: string, visibility: VisibilityEnum }) {
        this.visibility = data.visibility;
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    onConfirm(): void {
        this.dialogRef.close({ visibility: this.visibility  });
    }
}
