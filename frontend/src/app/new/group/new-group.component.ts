import { Component } from '@angular/core';
import { FormControl, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { map, startWith } from 'rxjs/operators';
import { BridgeService } from '../../bridges/bridge.service';
import { ConnectionService } from '../../connection.service';
import { MonitorService } from '../../monitor.service';
import { ProgramService } from '../../program.service';
import { ServiceService } from '../../service.service';
import { Session } from '../../session';
import { SessionService } from '../../session.service';
import { MatSnackBar } from '@angular/material/snack-bar';


@Component({
    // moduleId: module.id,
    selector: 'app-my-new-group',
    templateUrl: './new-group.component.html',
    providers: [BridgeService, ConnectionService, MonitorService, ProgramService, SessionService, ServiceService],
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
    filteredOptions: Observable<{name: string}[]>;
    invitationSearch = new FormControl();
    testSearchOpts = [{name: 'one'}, {name: 'two'}, {name: 'three'}];
    errorMessage: string = '';
    groupErrorMessage: string = '';

    constructor(
        public sessionService: SessionService,
        public router: Router,
        public dialog: MatDialog,
        private _snackBar: MatSnackBar,
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

                    this.filteredOptions = this.invitationSearch.valueChanges
                        .pipe(
                            startWith(''),
                            map(value => typeof value === 'string' ? value : value.name),
                            map(name => name ? this._filterInvitationName(name) : this.testSearchOpts.slice())
                        );
                }
            })
            .catch(e => {
                console.log('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    validateGroupName() {

    }

    displayInvitation(user: {name: string}): string {
        return user && user.name ? user.name : '';
    }

    private _filterInvitationName(name: string): {name: string}[] {
        const filterValue = name.toLowerCase();

        return this.testSearchOpts.filter(option => option.name.toLowerCase().indexOf(filterValue) === 0);
    }

    selectOption(opt){
        this.invitationSearch.reset();
        this._snackBar.open("Selected " + opt.name, 'ok', {
            duration: 2000,
        });
    }
}
