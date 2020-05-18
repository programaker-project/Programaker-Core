import { Component, Inject, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ConnectionService } from '../../connection.service';
import { ProgramType } from '../../program';
import { ServiceService } from '../../service.service';
import { SessionService } from '../../session.service';

@Component({
    selector: 'app-select-programming-model-dialog',
    templateUrl: './select-programming-model-dialog.component.html',
    styleUrls: ['./select-programming-model-dialog.component.scss'],
    providers: [SessionService, ServiceService, ConnectionService],
})
export class SelectProgrammingModelDialogComponent implements OnInit {
    selectedType: ProgramType = null;
    programName = new FormControl('', [Validators.required, Validators.minLength(4)]);

    constructor(public dialogRef: MatDialogRef<SelectProgrammingModelDialogComponent>,
                public sessionService: SessionService,
                public serviceService: ServiceService,
                public connectionService: ConnectionService,

                @Inject(MAT_DIALOG_DATA)
                public data: {  }) {
    }

    ngOnInit(): void {
    }


    getNameErrorMessage() {
        if (this.programName.hasError('required')) {
            return 'You must enter a value';
        }

        return this.programName.hasError('minlength') ? 'Name must have at least 4 characters' : '';
    }

    selectType(program_type: ProgramType) {
        this.selectedType = program_type;
    }

    confirmSelection() {
        this.dialogRef.close({success: true, program_type: this.selectedType, program_name: this.programName.value});
    }

    onNoClick(): void {
        this.dialogRef.close({success: false, program_type: null, program_name: null});
    }
}
