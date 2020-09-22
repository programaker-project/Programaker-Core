import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { ServiceService } from '../service.service';
import { Session } from '../session';
import { SessionService } from '../session.service';
import { BridgeMetadata } from './bridge';
import { BridgeService } from './bridge.service';
import { toWebsocketUrl } from 'app/utils';




@Component({
    // moduleId: module.id,
    selector: 'bridge-add-component',
    templateUrl: './add.component.html',
    providers: [ServiceService, BridgeService],
})

export class BridgeAddComponent {
    session: Session;
    bridgeName = "";
    portControlUrl = "";
    editable = true;

    constructor(
        private sessionService: SessionService,
        private bridgeService: BridgeService,
        private router: Router,
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
            });
    }

    create(): void {
        this.editable = false;
        this.bridgeService.createServicePort(this.bridgeName).then((BridgeMetadata: BridgeMetadata) => {
            this.portControlUrl = toWebsocketUrl(BridgeMetadata.control_url);
        }).catch(() => {
            this.editable = true;
        });
    }

    back(): void {
        history.back();
    }
}
