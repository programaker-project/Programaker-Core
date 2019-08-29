import { Component, Inject } from '@angular/core';
import { ProgramMetadata } from './program';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'stop-thread-program-dialog',
    templateUrl: 'stop-thread-program-dialog.html',
    styleUrls: [
        'stop-thread-program-dialog.css',
    ]
})

export class StopThreadProgramDialogComponent {
    program: ProgramMetadata;
    program_name: string;

    constructor(public dialogRef: MatDialogRef<StopThreadProgramDialogComponent>,
                @Inject(MAT_DIALOG_DATA)
                public data: ProgramMetadata) {

        this.program = data;
        this.program_name = this.program.name;
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
