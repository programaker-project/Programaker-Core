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

import { TemplateService } from './template.service';
import { Template } from './template';


@Component({
    // moduleId: module.id,
    selector: 'template-create-component',
    templateUrl: './create.component.html',
    providers: [ServiceService, TemplateService],
})

export class TemplateCreateComponent {
    session: Session;
    portName = "";
    portControlUrl = "";
    editable = true;

    constructor(
        private sessionService: SessionService,
        private bridgeService: TemplateService,
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

    back(): void {
        history.back();
    }
}
