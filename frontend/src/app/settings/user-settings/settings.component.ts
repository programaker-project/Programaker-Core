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
import { getUserPictureUrl, iconDataToUrl } from 'app/utils';
import { EnvironmentService } from 'app/environment.service';
import { ProgramMetadata } from 'app/program';
import { GroupInfo } from 'app/group';
import { BridgeIndexData } from 'app/bridges/bridge';
import { UserProfileInfo } from 'app/profiles/profile.service';

@Component({
    // moduleId: module.id,
    selector: 'app-my-settings',
    templateUrl: './settings.component.html',
    providers: [BridgeService, ConnectionService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'settings.component.scss',
        '../../libs/css/material-icons.css',
        '../../libs/css/bootstrap.min.css',
    ],
})
export class SettingsComponent {
    session: Session;
    is_advanced: boolean;
    is_in_preview: boolean;

    loadedImage: File = null;

    @ViewChild('imgPreview') imgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('imgFileInput') imgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('saveAvatarButton') saveAvatarButton: MatButton;

    readonly _getUserPicture: (userId: string) => string;

    groups: GroupInfo[];
    listedGroups: {[key: string]: boolean} = {};

    bridges: BridgeIndexData[];

    constructor(
        public sessionService: SessionService,
        public router: Router,
        private route: ActivatedRoute,
        private environmentService: EnvironmentService,

        public dialog: MatDialog,
    ) {
        this._getUserPicture = getUserPictureUrl.bind(this, environmentService);

        this.route.data
            .subscribe((data: { groups: GroupInfo[], user_profile: UserProfileInfo }) => {
                this.groups = data.groups;

                for (const group of data.user_profile.groups) {
                    this.listedGroups[group.id] = true;
                }
            });
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
                        this.is_in_preview = this.session.tags.is_in_preview;
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

    onChangeInPreviewSettings(event: MatSlideToggleChange) {
        this.is_in_preview = event.checked;
    }

    async saveUserSettings() {
        // Send update
        const button = document.getElementById('user-settings-save-button');
        if (button){
            button.classList.add('started');
            button.classList.remove('completed');
        }

        const publicGroups = [];
        for (const group of Object.keys(this.listedGroups)) {
            if (this.listedGroups[group]) {
                publicGroups.push(group);
            }
        }

        await this.sessionService.updateUserProfileSettings({ groups: publicGroups });

        const userSettings: {is_advanced: boolean, is_in_preview?: boolean} = {
            is_advanced: this.is_advanced,
        };

        if (this.is_advanced) {
            userSettings.is_in_preview = this.is_in_preview;
        }

        const success = await this.sessionService.updateUserSettings(userSettings);

        if (success) {
            this.session = await this.sessionService.forceUpdateSession();
            console.log("Session", this.session);
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
