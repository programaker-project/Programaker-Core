import { Component, Inject } from '@angular/core';
import { SessionService } from '../session.service';
import { ConnectionService } from '../connection.service';
import { ServiceService } from '../service.service';
import { BridgeService } from '../bridges/bridge.service';

import { HowToEnableServiceDialogComponent } from '../HowToEnableServiceDialogComponent';
import { AvailableService, ServiceEnableHowTo } from '../service';


import { MatDialog } from '@angular/material/dialog';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeIndexData } from '../bridges/bridge';

type BridgeData = {bridge: BridgeIndexData, state: 'waiting' | 'reading' | 'selected' | 'error', index: number};

@Component({
    selector: 'app-add-connection-dialog',
    templateUrl: 'add-connection-dialog.component.html',
    styleUrls: [
        'add-connection-dialog.component.css',
    ],
    providers: [BridgeService, SessionService, ServiceService, ConnectionService],
})
export class AddConnectionDialogComponent {
    availableBridges: BridgeData[] = [];
    selectedIndex = null;

    constructor(public dialogRef: MatDialogRef<AddConnectionDialogComponent>,
                public sessionService: SessionService,
                public serviceService: ServiceService,
                public connectionService: ConnectionService,
                public dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: {  }) {
        this.connectionService.getAvailableBridgesForNewConnection().then((bridges: BridgeIndexData[]) => {
            console.log("Bridges:", bridges);
            for (let i = 0 ; i < bridges.length; i++){
                this.availableBridges.push({ bridge: bridges[i], state: 'waiting', index: i });
            }
        });
    }

    onNoClick(): void {
        this.dialogRef.close();
    }

    enableService(service: BridgeData): void {
        if ((this.selectedIndex === service.index)
            && (this.availableBridges[this.selectedIndex].state !== 'error')) {
            return;
        }

        if (this.selectedIndex !== null) {
            if (this.availableBridges[this.selectedIndex].state === 'selected') {
                this.availableBridges[this.selectedIndex].state = 'waiting';
            }
        }

        this.selectedIndex = service.index;
        if (service.state !== 'reading') {
            service.state = 'reading';
            this.serviceService.getHowToEnable(this.connectionService.toAvailableService(service.bridge))
                .then(howToEnable => {
                    if (this.selectedIndex === service.index) {
                        service.state = 'selected';
                    }

                    this.showHowToEnable(service, howToEnable)
                }).catch(err => {
                    service.state = 'error';
                    console.error(err);
                });
        }
    }

    showHowToEnable(service: BridgeData, howTo: ServiceEnableHowTo): void {
        service.state = 'selected';

        if ((howTo === null) || (howTo === undefined)) {
            // No configuration, just accept
            return;
        }

        console.log(howTo);
        const dialogRef = this.dialog.open(HowToEnableServiceDialogComponent, {
            data: howTo
        }).afterClosed().subscribe({
            next: () => {
                this.availableBridges[this.selectedIndex].state = 'waiting';
                this.selectedIndex = null;
            }
        });
    }

}
