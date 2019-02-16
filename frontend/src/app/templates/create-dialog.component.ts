import { Component, Inject } from '@angular/core';
import { ServiceEnableHowTo, ServiceEnableMessage, ServiceEnableEntry, ServiceEnableType } from '../service';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SessionService } from '../session.service';
import { ServiceService } from '../service.service';
import { Template } from './template';

@Component({
    selector: 'template-create-dialog-component',
    templateUrl: './create-dialog.component.html',
    styleUrls: [
        './create-dialog.component.css',
    ],
    providers: [SessionService, ServiceService],
})

export class TemplateCreateDialogComponent {
    template: Template;

    constructor(
        public dialogRef: MatDialogRef<TemplateCreateDialogComponent>,
        public serviceService: ServiceService,
        @Inject(MAT_DIALOG_DATA)
        public data: { template: Template }
    ) {
        this.template = data.template;
    }


    onNoClick(): void {
        this.dialogRef.close();
    }
}
