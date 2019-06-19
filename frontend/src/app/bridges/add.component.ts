import { Component, OnInit } from '@angular/core';
import { Service, RequestInput, Request } from '../service';
import { ServiceService } from '../service.service';
import { Location } from '@angular/common';
import { Http } from '@angular/http';
import { GetTypeOfJson, JSONType } from '../json';
import { MatSnackBar } from '@angular/material';
import { Router } from '@angular/router';

import { Session } from '../session';
import { SessionService } from '../session.service';

import { BridgeService } from './bridge.service';
import { BridgeMetadata } from './bridge';


@Component({
    // moduleId: module.id,
    selector: 'bridge-add-component',
    templateUrl: './add.component.html',
    providers: [ServiceService, BridgeService],
})

export class BridgeAddComponent {
    session: Session;
    portName = "";
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
                    this.router.navigate(['/login']);
                }
            });
    }

    create(): void {
        this.editable = false;
        this.bridgeService.createServicePort(this.portName).then((BridgeMetadata: BridgeMetadata) => {
            this.portControlUrl = document.location.origin + BridgeMetadata.control_url;
        }).catch(() => {
            this.editable = true;
        });
    }

    back(): void {
        history.back();
    }
}
