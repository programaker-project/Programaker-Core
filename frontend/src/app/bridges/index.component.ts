import { Component } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { AddBridgeDialogComponent } from 'app/dialogs/add-bridge-dialog/add-bridge-dialog.component';
import { UpdateBridgeDialogComponent } from 'app/dialogs/update-bridge-dialog/update-bridge-dialog.component';
import { ServiceService } from '../service.service';
import { Session } from '../session';
import { SessionService } from '../session.service';
import { BridgeIndexData } from './bridge';
import { BridgeService } from './bridge.service';


@Component({
    // moduleId: module.id,
    selector: 'bridge-index-component',
    templateUrl: './index.component.html',
    providers: [ServiceService, BridgeService],
    styleUrls: [
        './index.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})

export class BridgeIndexComponent {
    session: Session;
    bridges: BridgeIndexData[];
    expandedBridgeId: string;

    constructor(
        private sessionService: SessionService,
        private bridgeService: BridgeService,
        private router: Router,
        public dialog: MatDialog,
    ) {
        this.sessionService = sessionService;
        this.bridgeService = bridgeService;
        this.router = router;
    }

    // tslint:disable-next-line:use-life-cycle-interface
    ngOnInit(): void {
        this.sessionService.getSession()
            .then(session => {
                this.session = session;
                if (!session.active) {
                    this.router.navigate(['/login'], {replaceUrl:true});
                }
                else {
                    this.refreshBridgeList();
                }
            });
    }

    refreshBridgeList(): void {
        this.bridgeService.listUserBridges().then((data) => {
            this.bridges = data.bridges;
        });
    }

    addBridge(): void {
        const dialogRef = this.dialog.open(AddBridgeDialogComponent, { width: '50%',
                                                                       data: { },
                                                                     });

        dialogRef.afterClosed().subscribe((result: {success: boolean, bridgeId?: string, bridgeName?: string}) => {
            if (result && result.success) {
                this.refreshBridgeList();

                this.showBridgeDetail({ id: result.bridgeId, name: result.bridgeName });
            }
        });

    }

    showBridgeDetail(bridge: { id: string, name: string }): void {
        const dialogRef = this.dialog.open(UpdateBridgeDialogComponent, { width: '90%',
                                                                          maxHeight: '100vh',
                                                                          autoFocus: false,
                                                                          data: { bridgeInfo: bridge,
                                                                                },
                                                                        });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.refreshBridgeList();
            }
        });
    }
}
