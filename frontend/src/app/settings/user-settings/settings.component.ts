import * as progbar from 'app/ui/progbar';

import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { Router, ActivatedRoute } from '@angular/router';

import { ProgramService } from 'app/program.service';

import { Session } from 'app/session';
import { SessionService } from 'app/session.service';

import { ServiceService } from 'app/service.service';
import { MatDialog } from '@angular/material/dialog';
import { MatSlideToggleChange } from '@angular/material/slide-toggle';

import { MonitorService } from 'app/monitor.service';
import { ConnectionService } from 'app/connection.service';
import { BridgeService } from 'app/bridges/bridge.service';
import { MatButton } from '@angular/material/button';
import { getUserPictureUrl } from 'app/utils';

@Component({
    // moduleId: module.id,
    selector: 'app-my-settings',
    templateUrl: './settings.component.html',
    providers: [BridgeService, ConnectionService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'settings.component.css',
        '../../libs/css/material-icons.css',
        '../../libs/css/bootstrap.min.css',
    ],
})
export class SettingsComponent {
    session: Session;
    is_advanced: boolean;

    loadedImage: File = null;

    @ViewChild('imgPreview') imgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('imgFileInput') imgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('saveAvatarButton') saveAvatarButton: MatButton;

    _getUserPicture = getUserPictureUrl;

    constructor(
        public sessionService: SessionService,
        public router: Router,
        private route: ActivatedRoute,
        public dialog: MatDialog,
    ) {
    }

    // tslint:disable-next-line:use-life-cycle-interface
    ngOnInit(): void {
        this.route.data
            .subscribe({
                next: (data: { session: Session }) => {
                    this.session = data.session;
                    if (!data.session.active) {
                        this.router.navigate(['/login'], {replaceUrl:true});
                    }
                    else {
                        this.is_advanced = this.session.tags.is_advanced;
                    }
                },
                error: err => {
                    console.log('Error getting session', err);
                    this.router.navigate(['/login'], {replaceUrl:true});
                }
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

    previewImage(event: KeyboardEvent) {
        const input: HTMLInputElement = event.target as HTMLInputElement;

        if (input.files && input.files[0]) {
            const reader = new FileReader();

            reader.onload = (e) => {
                this.loadedImage = input.files[0];
                this.imgPreview.nativeElement.src = e.target.result as string;
            }

            reader.readAsDataURL(input.files[0]);
        }
    }

    async saveUserAvatar() {
        const buttonClass = this.saveAvatarButton._elementRef.nativeElement.classList;
        buttonClass.add('started');
        buttonClass.remove('completed');

        await this.sessionService.updateUserAvatar(this.loadedImage);

        buttonClass.remove('started');
        buttonClass.add('completed');
    }
}
