import { Component, ElementRef, ViewChild } from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatAutocomplete, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatButton } from '@angular/material/button';
import { MatDialog } from '@angular/material/dialog';
import { MatSlideToggleChange } from '@angular/material/slide-toggle';
import { ActivatedRoute, Router } from '@angular/router';
import { BridgeService } from 'app/bridges/bridge.service';
import { ConnectionService } from 'app/connection.service';
import { GroupInfo } from 'app/group';
import { GroupService, UserAutocompleteInfo } from 'app/group.service';
import { MonitorService } from 'app/monitor.service';
import { ProgramService } from 'app/program.service';
import { ServiceService } from 'app/service.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { Collaborator, CollaboratorRole, roleToIcon } from 'app/types/collaborator';
import { getGroupPictureUrl } from 'app/utils';
import { ConfirmDeleteDialogComponent } from 'app/dialogs/confirm-delete-dialog/confirm-delete-dialog.component';

const DEFAULT_ROLE : CollaboratorRole = 'editor';

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
    setToPublicGroup = null;

    @ViewChild('imgPreview') imgPreview: ElementRef<HTMLImageElement>;
    @ViewChild('imgFileInput') imgFileInput: ElementRef<HTMLInputElement>;
    @ViewChild('saveAvatarButton') saveAvatarButton: MatButton;

    @ViewChild('invitationAutocomplete') invitationAutocomplete: MatAutocomplete;
    @ViewChild('saveCollaboratorsButton') saveCollaboratorsButton: MatButton;
    @ViewChild('saveAdminButton') saveAdminButton: MatButton;
    @ViewChild('deletegroupButton') deletegroupButton: MatButton;

    invitationSearch = new FormControl();
    userNameQuery: Promise<UserAutocompleteInfo[]> | null = null;
    filteredOptions: UserAutocompleteInfo[];
    collaborators: Collaborator[];

    readonly _getGroupPicture = getGroupPictureUrl;
    readonly _roleToIcon = roleToIcon;

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
                    this.groupService.getGroupWithName(groupName).then((groupInfo) => {
                        this.groupInfo = groupInfo;
                        this.setToPublicGroup = this.groupInfo.public;
                    }).then(() => {
                        this.groupService.getCollaboratorsOnGroup(this.groupInfo.id).then(collaborators => {
                            this.collaborators = collaborators
                            this._setupAutocomplete()
                        });

                    });
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    private _setupAutocomplete() {
        // Update user list on invitation section
        this.invitationSearch.valueChanges.subscribe({
            next: (value) => {
                if ((!value) || (typeof value !== 'string')) {
                    this.filteredOptions = [];
                }
                else {
                    const query = this.userNameQuery = this.groupService.autocompleteUsers(value);

                    this.userNameQuery.then((result: UserAutocompleteInfo[]) => {
                        if (query !== this.userNameQuery) {
                            // No longer applicable
                            return;
                        }

                        result = result.filter((user) => {
                            if (user.id === this.session.user_id){
                                return false; // This is the user creating the group
                            }
                            if (this.collaborators.find(collaborator => collaborator.id === user.id)) {
                                return false; // This user is already on the list
                            }

                            return true;
                        });

                        this.filteredOptions = result;
                    });
                }
            }
        })
        this.invitationAutocomplete.optionSelected.subscribe({ next: this.selectOption.bind(this) });
    }

    onChangeAdvancedSettings(event: MatSlideToggleChange) {
        this.is_advanced = event.checked;
    }

    async saveCollaborators() {
        const buttonClass = this.saveCollaboratorsButton._elementRef.nativeElement.classList;
        buttonClass.add('started');
        buttonClass.remove('completed');

        await this.groupService.updateGroupCollaboratorList(this.groupInfo.id, this.collaborators)

        buttonClass.remove('started');
        buttonClass.add('completed');
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
        this.loadedImage = null;

        buttonClass.remove('started');
        buttonClass.add('completed');
    }

    async saveAdminSettings() {
        const buttonClass = this.saveAdminButton._elementRef.nativeElement.classList;
        buttonClass.add('started');
        buttonClass.remove('completed');

        const tasks: Promise<any>[] = [];
        if (this.groupInfo.public !== this.setToPublicGroup) {
            tasks.push(this.groupService.setPublicStatus(this.groupInfo.id, this.setToPublicGroup).then(() => {
                this.groupInfo.public = this.setToPublicGroup;
            }));
        }

        await Promise.all(tasks);

        buttonClass.remove('started');
        buttonClass.add('completed');
    }


    displayInvitation(user: {username: string}): string {
        return user && user.username ? user.username : '';
    }

    selectOption(arg: MatAutocompleteSelectedEvent){
        this.invitationSearch.reset();
        const collaborator = arg.option.value as Collaborator;
        collaborator.role = DEFAULT_ROLE;

        this.collaborators.push(collaborator);
    }

    removeCollaborator(user: UserAutocompleteInfo) {
        this.collaborators = this.collaborators.filter(collaborator => collaborator.id !== user.id);
    }


    startDeleteGroup() {
        const programData = { name: this.groupInfo.name };

        const dialogRef = this.dialog.open(ConfirmDeleteDialogComponent, {
            data: programData
        });

        dialogRef.afterClosed().subscribe(result => {
            if (!result) {
                console.log("Cancelled");
                return;
            }

            const deletion = (this.groupService.deleteGroup(this.groupInfo.id)
                .then(() => this.router.navigateByUrl("/"))
                .catch(() => { return false; }));
        });
    }

}
