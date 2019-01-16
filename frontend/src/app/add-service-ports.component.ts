import { Component, OnInit } from '@angular/core';
import { Service, RequestInput, Request } from './service';
import { ServiceService } from './service.service';
import { Location } from '@angular/common';
import { Http } from '@angular/http';
import { GetTypeOfJson, JSONType } from './json';
import { MatSnackBar } from '@angular/material';
import { Router } from '@angular/router';

import { Session } from './session';
import { SessionService } from './session.service';

import { ServicePortService } from './service_port.service';
import { ServicePortMetadata } from './service_port';


@Component({
    // moduleId: module.id,
    selector: 'app-my-add-service-port',
    templateUrl: './add-service-ports.component.html',
    providers: [ServiceService, ServicePortService],
})

export class AddServicePortsComponent {
    session: Session;
    portName = "";
    portControlUrl = "";
    editable = true;

    constructor(
        private sessionService: SessionService,
        private servicePortService: ServicePortService,
        private router: Router,
    ) {
        this.sessionService = sessionService;
        this.servicePortService = servicePortService;
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
        this.servicePortService.createServicePort(this.portName).then((ServicePortMetadata: ServicePortMetadata) => {
            this.portControlUrl = document.location.origin + ServicePortMetadata.control_url;
        }).catch(() => {
            this.editable = true;
        });
    }
}
