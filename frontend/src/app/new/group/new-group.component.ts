import { Location } from '@angular/common';
import { Component, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { GroupCollaboratorEditorComponent } from 'app/components/group-collaborator-editor/group-collaborator-editor.component';
import { GroupService } from 'app/group.service';
import { BridgeService } from '../../bridges/bridge.service';
import { ConnectionService } from '../../connection.service';
import { MonitorService } from '../../monitor.service';
import { ProgramService } from '../../program.service';
import { ServiceService } from '../../service.service';
import { Session } from '../../session';
import { SessionService } from '../../session.service';
import { canonicalizableValidator } from './new-group.component.validators';
import { GroupInfo } from 'app/group';

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
    processing = false;

    @ViewChild('groupCollaboratorEditor') groupCollaboratorEditor: GroupCollaboratorEditorComponent;

    constructor(
        private sessionService: SessionService,
        private groupService: GroupService,
        private router: Router,
        private dialog: MatDialog,
        private formBuilder: FormBuilder,
        private _location: Location,
    ) {
        this.options = this.formBuilder.group({
            groupName: ['', [Validators.required, Validators.minLength(4), canonicalizableValidator()]],
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
                }
            })
            .catch(e => {
                console.error('Error getting session', e);
                this.router.navigate(['/login'], {replaceUrl:true});
            });
    }

    validateGroupName() {
    }

    displayInvitation(user: {username: string}): string {
        return user && user.username ? user.username : '';
    }

    createGroup() {
        const groupName = this.options.controls.groupName.value;
        const isPublicGroup = this.publicGroup;
        const collaborators = this.groupCollaboratorEditor.getCollaborators();

        this.processing = true;
        this.groupService.createGroup(groupName, {
            'public': isPublicGroup,
            collaborators: collaborators.map(user => { return { id: user.id, role: user.role }; })
        })
            .then((group: GroupInfo) => {
                this.router.navigate(['/groups/' + group.name]);
            })
            .catch(err => {
                if ((err.name === 'HttpErrorResponse') && (err.status === 409)) {
                    this.errorMessage = 'A group already exists with this name.';
                    // TODO: Properly integrate with group name validations
                }
                else {
                    console.error(err);
                }
                this.processing = false;
            });
    }

    goBack() {
        this._location.back();
    }
}
