import { Component, Inject } from '@angular/core';
import { ProgramMetadata } from './program';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'app-delete-program-dialog',
    templateUrl: 'delete-program-dialog.html',
    styleUrls: [
        'delete-program-dialog.css',
    ]
})

export class DeleteProgramDialogComponent {
    program: ProgramMetadata;
    program_name: string;

    constructor(public dialogRef: MatDialogRef<DeleteProgramDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: ProgramMetadata) {

        this.program = data;
        this.program_name = this.program.name;
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
