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
    is_public_profile: boolean = true;

    loadedImage: File = null;

    @ViewChild('imgPreview') imgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('imgFileInput') imgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('saveAvatarButton') saveAvatarButton: MatButton;

    readonly _getUserPicture: (userId: string) => string;
    programs: ProgramMetadata[];
    publicPrograms: ProgramMetadata[];
    listedPrograms: {[key: string]: boolean} = {};
    bridgeInfo: { [key:string]: { icon: string, name: string }} = {};

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
            .subscribe((data: { programs: ProgramMetadata[], groups: GroupInfo[], bridges: BridgeIndexData[], user_profile: UserProfileInfo }) => {
                this.programs = data.programs.sort((a, b) => {
                    return a.name.localeCompare(b.name, undefined, { ignorePunctuation: true, sensitivity: 'base' });
                });
                this.publicPrograms = this.programs.filter(p => p.enabled && p.is_public);

                this.groups = data.groups;

                this.bridges = data.bridges;
                for (const bridge of this.bridges) {
                    this.bridgeInfo[bridge.id] = {
                        name: bridge.name,
                        icon: iconDataToUrl(this.environmentService, bridge.icon, bridge.id)
                    };
                }

                for (const group of data.user_profile.groups) {
                    this.listedGroups[group.id] = true;
                }

                for (const program of data.user_profile.programs) {
                    this.listedPrograms[program.id] = true;
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
                        this.is_public_profile = this.session.tags.is_public_profile;
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

    onChangePublicProfileSettings(event: MatSlideToggleChange) {
        this.is_public_profile = event.checked;
    }

    async saveUserSettings() {
        // Send update
        const button = document.getElementById('user-settings-save-button');
        if (button){
            button.classList.add('started');
            button.classList.remove('completed');
        }

        if (this.is_public_profile) {
            const publicPrograms = [];
            for (const prog of Object.keys(this.listedPrograms)) {
                if (this.listedPrograms[prog]) {
                    publicPrograms.push(prog);
                }
            }

            const publicGroups = [];
            for (const group of Object.keys(this.listedGroups)) {
                if (this.listedGroups[group]) {
                    publicGroups.push(group);
                }
            }

            await this.sessionService.updateUserProfileSettings({ programs: publicPrograms, groups: publicGroups });
        }

        const success = await this.sessionService.updateUserSettings({
            is_advanced: this.is_advanced,
            is_public_profile: this.is_public_profile,
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
