import { Component, ElementRef, ViewChild } from '@angular/core';
import { MatButton } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';
import { MatSlideToggleChange } from '@angular/material/slide-toggle';
import { Router, ActivatedRoute } from '@angular/router';
import { BridgeService } from 'app/bridges/bridge.service';
import { ConnectionService } from 'app/connection.service';
import { GroupInfo } from 'app/group';
import { MonitorService } from 'app/monitor.service';
import { ProgramService } from 'app/program.service';
import { ServiceService } from 'app/service.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { getGroupPictureUrl } from 'app/utils';
import { GroupService } from 'app/group.service';


@Component({
    // moduleId: module.id,
    selector: 'app-my-group-settings',
    templateUrl: './group-settings.component.html',
    providers: [BridgeService, ConnectionService, GroupService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'group-settings.component.css',
        '../../libs/css/material-icons.css',
        '../../libs/css/bootstrap.min.css',
    ],
})
export class GroupSettingsComponent {
    session: Session;
    is_advanced: boolean;
    groupInfo: GroupInfo;

    loadedImage: File = null;

    @ViewChild('imgPreview') imgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('imgFileInput') imgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('saveAvatarButton') saveAvatarButton: MatButton;

    _getGroupPicture = getGroupPictureUrl;

    constructor(
        public sessionService: SessionService,
        private route: ActivatedRoute,
        public router: Router,
        public dialog: MatDialog,
        private groupService: GroupService,
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

                    const params = this.route.params['value'];
                    const groupName = params['group_name'];
                    this.groupService.getGroupWithName(groupName).then(groupInfo => this.groupInfo = groupInfo);
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

    async saveSettings() {
        // Send update
        const button = document.getElementById('user-settings-save-button');
        if (button){
            button.classList.add('started');
            button.classList.remove('completed');
        }

        // const success = await this.sessionService.updateUserSettings({
        //     is_advanced: this.is_advanced,
        // });

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

    async saveAvatar() {
        const buttonClass = this.saveAvatarButton._elementRef.nativeElement.classList;
        buttonClass.add('started');
        buttonClass.remove('completed');

        await this.groupService.updateGroupAvatar(this.groupInfo.id, this.loadedImage);

        buttonClass.remove('started');
        buttonClass.add('completed');
    }
}
