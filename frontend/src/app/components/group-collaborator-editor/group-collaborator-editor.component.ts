import { Component, Input, OnInit, ViewChild } from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatAutocomplete, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';
import { GroupService, UserAutocompleteInfo } from 'app/group.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { Collaborator, CollaboratorRole, roleToIcon } from 'app/types/collaborator';

const DEFAULT_ROLE : CollaboratorRole = 'editor';

@Component({
    selector: 'app-group-collaborator-editor',
    templateUrl: './group-collaborator-editor.component.html',
    styleUrls: [
        './group-collaborator-editor.component.scss',
        '../../libs/css/material-icons.css',
    ],
    providers: [SessionService, GroupService],
})
export class GroupCollaboratorEditorComponent implements OnInit {
    @Input() collaborators: Collaborator[] = [];

    @ViewChild('invitationAutocomplete') invitationAutocomplete: MatAutocomplete;
    invitationSearch = new FormControl();
    userNameQuery: Promise<UserAutocompleteInfo[]> | null = null;
    filteredOptions: UserAutocompleteInfo[];
    session: Session;

    readonly _roleToIcon = roleToIcon;

    constructor(
        private groupService: GroupService,
        private sessionService: SessionService,
    ) {
    }

    async ngOnInit(): Promise<void> {
        this.session = await this.sessionService.getSession();
        this._setupAutocomplete();
    }

    public getCollaborators(): Collaborator[] {
        return this.collaborators;
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
