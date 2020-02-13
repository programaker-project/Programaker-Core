import { Component, Inject } from '@angular/core';
import { SessionService } from '../session.service';
import { ConnectionService } from '../connection.service';
import { ServiceService } from '../service.service';

import { HowToEnableServiceDialogComponent } from '../HowToEnableServiceDialogComponent';
import { AvailableService, ServiceEnableHowTo } from '../service';


import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'app-add-connection-dialog',
    templateUrl: 'add-connection-dialog.component.html',
    styleUrls: [
        'add-connection-dialog.component.css',
    ],
    providers: [SessionService, ServiceService, ConnectionService],
})

export class AddConnectionDialogComponent {
    availableBridges: AvailableService[] = [];

    constructor(public dialogRef: MatDialogRef<AddConnectionDialogComponent>,
                public sessionService: SessionService,
                public serviceService: ServiceService,
                public connectionService: ConnectionService,
                @Inject(MAT_DIALOG_DATA)
                public data: {  }) {
        this.connectionService.getAvailableBridges().then((bridges: AvailableService[]) => {
            console.log("Bridges:", bridges);
            this.availableBridges = bridges;
        });
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    enableService(service: AvailableService): void {
        this.serviceService.getHowToEnable(service)
            .then(howToEnable => this.showHowToEnable(howToEnable));
    }

    showHowToEnable(howTo: ServiceEnableHowTo): void {
        if ((howTo as any).success === false) {
            return;
        }
        console.log(howTo);
    }

}
