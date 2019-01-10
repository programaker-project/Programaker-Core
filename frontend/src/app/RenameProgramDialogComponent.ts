import { Component, Inject } from '@angular/core';
import { ProgramMetadata } from './program';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'app-rename-program-dialog',
    templateUrl: 'rename-program-dialog.html',
    styleUrls: [
        'rename-program-dialog.css',
    ]
})

export class RenameProgramDialogComponent {
    program: ProgramMetadata;
    program_name: string;

    constructor(public dialogRef: MatDialogRef<RenameProgramDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: ProgramMetadata) {

        this.program = data;
        this.program_name = this.program.name;
    }
    onNoClick(): void {
        this.dialogRef.close();
    }
}
