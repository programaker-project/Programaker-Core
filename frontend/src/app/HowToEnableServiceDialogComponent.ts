import { Component, Inject } from '@angular/core';
import { ServiceEnableHowTo, ServiceExtraTelegramInfo } from './service';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'app-how-to-enable-service-dialog',
    templateUrl: 'how-to-enable-service-dialog.html',
    styleUrls: [
        'how-to-enable-service-dialog.css',
    ]
})

export class HowToEnableServiceDialogComponent {
    service: ServiceEnableHowTo;
    bot_link: string;
    constructor(public dialogRef: MatDialogRef<HowToEnableServiceDialogComponent>,
        @Inject(MAT_DIALOG_DATA)
        public data: ServiceEnableHowTo) {
        this.service = data;
        this.bot_link = 'https://telegram.me/' + (data.extra as ServiceExtraTelegramInfo).bot_name;
    }
    onNoClick(): void {
        this.dialogRef.close();
    }
}
