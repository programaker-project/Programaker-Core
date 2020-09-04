import { Component, ViewChild, ElementRef } from '@angular/core';
import { FormControl, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { map, startWith, filter } from 'rxjs/operators';
import { BridgeService } from '../../bridges/bridge.service';
import { ConnectionService } from '../../connection.service';
import { MonitorService } from '../../monitor.service';
import { ProgramService } from '../../program.service';
import { ServiceService } from '../../service.service';
import { Session } from '../../session';
import { SessionService } from '../../session.service';
import { MatSnackBar } from '@angular/material/snack-bar';
import { GroupService, UserAutocompleteInfo } from 'app/group.service';
import { MatAutocomplete, MatAutocompleteSelectedEvent } from '@angular/material/autocomplete';


@Component({
    // moduleId: module.id,
    selector: 'app-my-new-group',
    templateUrl: './new-group.component.html',
    providers: [BridgeService, ConnectionService, GroupService, MonitorService, ProgramService, SessionService, ServiceService],
    styleUrls: [
        'new-group.component.css',
        '../../libs/css/material-icons.css',
        '../../libs/css/bootstrap.min.css',
    ],
})
export class NewGroupComponent {
    session: Session;
    is_advanced: boolean;
    options: FormGroup;
    publicGroup: boolean = false;
    errorMessage: string = '';
    groupErrorMessage: string = '';

    @ViewChild('invitationAutocomplete') invitationAutocomplete: MatAutocomplete;
    invitationSearch = new FormControl();
    userNameQuery: Promise<UserAutocompleteInfo[]> | null = null;
    filteredOptions: UserAutocompleteInfo[];
    collaborators: UserAutocompleteInfo[] = [];

    constructor(
        public sessionService: SessionService,
        public router: Router,
        public dialog: MatDialog,
        private groupService: GroupService,
    ) {
        this.options = new FormGroup({
            groupName: new FormControl(),
        });
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
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    validateGroupName() {

    }

    displayInvitation(user: {username: string}): string {
        return user && user.username ? user.username : '';
    }

    selectOption(arg: MatAutocompleteSelectedEvent){
        this.invitationSearch.reset();
        this.collaborators.push(arg.option.value as UserAutocompleteInfo);
    }

    removeCollaborator(user: UserAutocompleteInfo) {
        this.collaborators = this.collaborators.filter(collaborator => collaborator.id !== user.id);
    }
}
