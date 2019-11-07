import { Component, OnInit } from '@angular/core';
import { Service, RequestInput, Request } from '../service';
import { ServiceService } from '../service.service';
import { Location } from '@angular/common';
import { Http } from '@angular/http';
import { GetTypeOfJson, JSONType } from '../json';
import { MatSnackBar } from '@angular/material';
import { Router } from '@angular/router';

import * as API from '../api-config';
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
                    this.router.navigate(['/login']);
                }
            });
    }

    get_websocket_root(): string {
        let baseServerPath = document.location.origin;
        if (API.ApiHost != '') {
            baseServerPath = API.ApiHost;
        }

        return this.to_websocket_path(baseServerPath);
    }

    to_websocket_path(basePath: string): string {
        // This two cases can be done with a single regexp
        //  but this is clearer
        if (basePath.startsWith('http://')) {
            return basePath.replace(/^http:\/\//, 'ws://')
        }
        else if (basePath.startsWith('https://')) {
            return basePath.replace(/^https:\/\//, 'wss://')
        }

        // Not sure how to convert this, so just return it
        return basePath;
    }

    create(): void {
        this.editable = false;
        this.bridgeService.createServicePort(this.bridgeName).then((BridgeMetadata: BridgeMetadata) => {
            this.portControlUrl = this.get_websocket_root() + BridgeMetadata.control_url;
        }).catch(() => {
            this.editable = true;
        });
    }

    back(): void {
        history.back();
    }
}
