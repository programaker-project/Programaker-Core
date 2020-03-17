import * as progbar from '../ui/progbar';

import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import { ProgramService } from '../program.service';

import { Session } from '../session';
import { SessionService } from '../session.service';

import { ServiceService } from '../service.service';
import { MatDialog } from '@angular/material/dialog';
import { MatSlideToggleChange } from '@angular/material/slide-toggle';

import { MonitorService } from '../monitor.service';
import { ConnectionService } from '../connection.service';
import { BridgeService } from '../bridges/bridge.service';

@Component({
    // moduleId: module.id,
    selector: 'app-my-settings',
    templateUrl: './settings.component.html',
    providers: [BridgeService, ConnectionService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'settings.component.css',
        '../libs/css/material-icons.css',
        '../libs/css/bootstrap.min.css',
    ],
})
export class SettingsComponent {
    private session: Session;
    is_advanced: boolean;

    constructor(
        public sessionService: SessionService,
        public router: Router,
        public dialog: MatDialog,
    ) {
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
                    this.is_advanced = this.session.tags.is_advanced;
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    onChangeAdvancedSettings(event: MatSlideToggleChange) {
        this.is_advanced = event.checked;
    }

    async saveUserSettings() {
        // Send update
        const button = document.getElementById('user-settings-save-button');
        if (button){
            button.classList.add('started');
            button.classList.remove('completed');
        }

        const success = await this.sessionService.updateUserSettings({
            is_advanced: this.is_advanced,
        });

        if (success) {
            this.session = await this.sessionService.forceUpdateSession();
        }

        if (button){
            button.classList.remove('started');
            button.classList.add('completed');
        }
    }
}
