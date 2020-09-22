import * as progbar from '../ui/progbar';

import { Component } from '@angular/core';
import { ServiceService } from '../service.service';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';

import { Session } from '../session';
import { SessionService } from '../session.service';

import { BridgeService } from './bridge.service';
import { BridgeIndexData } from './bridge';
import { BridgeDeleteDialogComponent } from './delete-dialog.component';
import { UpdateBridgeDialogComponent } from 'app/dialogs/update-bridge-dialog/update-bridge-dialog.component';


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
                    this.refresh_bridge_list();
                }
            });
    }

    refresh_bridge_list(): void {
        this.bridgeService.listUserBridges().then((data) => {
            this.bridges = data.bridges;
        });
    }

    addBridge(): void {
        this.router.navigate(['/bridges/add']);
    }

    showBridgeDetail(bridge: BridgeIndexData): void {
        const dialogRef = this.dialog.open(UpdateBridgeDialogComponent, { width: '90%',
                                                                          maxHeight: '100vh',
                                                                          data: { bridgeInfo: bridge,
                                                                                },
                                                                        });

        dialogRef.afterClosed().subscribe((result: {success: boolean}) => {
            if (result && result.success) {
                this.refresh_bridge_list();
            }
        });
    }
}
