import { Component, Inject } from '@angular/core';
import { SessionService } from '../session.service';
import { ConnectionService } from '../connection.service';
import { ServiceService } from '../service.service';
import { BridgeService } from '../bridges/bridge.service';

import { HowToEnableServiceDialogComponent } from '../HowToEnableServiceDialogComponent';
import { AvailableService, ServiceEnableHowTo, ServiceEnableMessage } from '../service';


import { MatDialog } from '@angular/material/dialog';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeIndexData } from '../bridges/bridge';

type BridgeData = {bridge: BridgeIndexData, state: 'waiting' | 'reading' | 'selected' | 'error', index: number};

@Component({
    selector: 'app-add-connection-dialog',
    templateUrl: 'add-connection-dialog.component.html',
    styleUrls: [
        'add-connection-dialog.component.css',
        '../libs/css/material-icons.css',
    ],
    providers: [BridgeService, SessionService, ServiceService, ConnectionService],
})
export class AddConnectionDialogComponent {
    availableBridges: BridgeData[] = null;
    selectedIndex = null;

    constructor(public dialogRef: MatDialogRef<AddConnectionDialogComponent>,
                public sessionService: SessionService,
                public serviceService: ServiceService,
                public connectionService: ConnectionService,
                public dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId?: string }) {
        if (!data) { data = this.data = {}; }


        let query: Promise<BridgeIndexData[]>;
        if (data.groupId) {
            query = this.connectionService.getAvailableBridgesForNewConnectionOnGroup(data.groupId);
        }
        else {
            query = this.connectionService.getAvailableBridgesForNewConnection();
        }

        query.then((bridges: BridgeIndexData[]) => {
            this.availableBridges = [];
            console.log("Bridges:", bridges);
            for (let i = 0 ; i < bridges.length; i++){
                this.availableBridges.push({ bridge: bridges[i], state: 'waiting', index: i });
            }
        });
    }

    onNoClick(): void {
        this.dialogRef.close({success: false});
    }

    async enableService(service: BridgeData): Promise<void> {
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
            const as_service = await this.connectionService.toAvailableService(service.bridge);

            let query: Promise<ServiceEnableHowTo>;
            if (this.data.groupId) {
                query = this.serviceService.getHowToEnableOnGroup(as_service, this.data.groupId);
            }
            else {
                query = this.serviceService.getHowToEnable(as_service)
            }

            query.then(howToEnable => {
                    if (this.selectedIndex === service.index) {
                        service.state = 'selected';
                    }

                    if (howToEnable.type === 'direct') {
                        return;
                    }

                    this.showHowToEnable(service, howToEnable as ServiceEnableMessage);
                }).catch(err => {
                    service.state = 'error';
                    console.error(err);
                });
        }
    }

    establishConnection(): void {
        if (this.selectedIndex === null) {
            return;
        }

        if (this.availableBridges[this.selectedIndex].state !== 'selected') {
            return;
        }

        console.log("Connecting to:", this.availableBridges[this.selectedIndex]);
        let query;
        if (this.data.groupId) {
            query = this.serviceService.directRegisterServiceOnGroup(this.availableBridges[this.selectedIndex].bridge.id, this.data.groupId)
        }
        else {
            query = this.serviceService.directRegisterService(this.availableBridges[this.selectedIndex].bridge.id)
        }

        query.then((result) => {
            console.log("Result:", result);
            if (result.success) {
                this.dialogRef.close({success: true});
            }
        }).catch((error) => {
            console.error("Error registering", error);
        });
    }


    showHowToEnable(service: BridgeData, howTo: ServiceEnableHowTo): void {
        service.state = 'selected';

        if ((howTo === null) || (howTo === undefined)) {
            // No configuration, just accept
            return;
        }

        console.log(howTo);
        const backchannel = {howTo, success: false};
        const dialogRef = this.dialog.open(HowToEnableServiceDialogComponent, {
            data: backchannel
        }).afterClosed().subscribe({
            next: () => {
                if (backchannel.success) {
                    this.dialogRef.close({success: true});
                }
                this.availableBridges[this.selectedIndex].state = 'waiting';
                this.selectedIndex = null;
            }
        });
    }

}
