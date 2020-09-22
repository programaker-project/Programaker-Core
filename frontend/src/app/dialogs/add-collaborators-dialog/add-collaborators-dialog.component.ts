import { Component, Inject, ViewChild } from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatAutocomplete, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { GroupService, UserAutocompleteInfo } from 'app/group.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { Collaborator, CollaboratorRole, roleToIcon } from 'app/types/collaborator';
import { MatButton } from '@angular/material/button';

const DEFAULT_ROLE : CollaboratorRole = 'editor';

@Component({
    selector: 'app-add-collaborators-dialog',
    templateUrl: 'add-collaborators-dialog.component.html',
    styleUrls: [
        'add-collaborators-dialog.component.css',
        '../../libs/css/material-icons.css',
    ],
    providers: [SessionService, GroupService],
})
export class AddCollaboratorsDialogComponent {
    @ViewChild('confirmationButton') confirmationButton: MatButton;
    @ViewChild('invitationAutocomplete') invitationAutocomplete: MatAutocomplete;
    invitationSearch = new FormControl();
    userNameQuery: Promise<UserAutocompleteInfo[]> | null = null;
    filteredOptions: UserAutocompleteInfo[];
    collaborators: Collaborator[] = [];
    session: Session;

    readonly _roleToIcon = roleToIcon;

    constructor(public dialogRef: MatDialogRef<AddCollaboratorsDialogComponent>,
                private groupService: GroupService,
                private sessionService: SessionService,
                private dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId: string, existingCollaborators: { id: string }[] }) {
    }

    async ngOnInit() {
        this.session = await this.sessionService.getSession();
        this._setupAutocomplete();
    }

    _setupAutocomplete() {
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
                            if (this.data.existingCollaborators.find(collaborator => collaborator.id === user.id)) {
                                return false; // Already a collaborator
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

    onNoClick(): void {
        this.dialogRef.close({success: false});
    }

    async confirm(): Promise<void> {
        // Indicate that the process has started
        const classList = this.confirmationButton._elementRef.nativeElement.classList;
        classList.add('started');
        classList.remove('completed');

        // Perform update
        await this.groupService.inviteUsers(this.data.groupId, this.collaborators);

        // Indicate that the process has ended
        classList.remove('started');
        classList.add('completed');

        this.dialogRef.close({success: true});
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
}
