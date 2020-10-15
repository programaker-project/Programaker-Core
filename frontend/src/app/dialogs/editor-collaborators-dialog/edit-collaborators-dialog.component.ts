import { Component, Inject, ViewChild } from '@angular/core';
import { MatButton } from '@angular/material/button';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { GroupCollaboratorEditorComponent } from 'app/components/group-collaborator-editor/group-collaborator-editor.component';
import { GroupService } from 'app/group.service';
import { SessionService } from 'app/session.service';

@Component({
    selector: 'app-edit-collaborators-dialog',
    templateUrl: 'edit-collaborators-dialog.component.html',
    styleUrls: [
        'edit-collaborators-dialog.component.css',
        '../../libs/css/material-icons.css',
    ],
    providers: [SessionService, GroupService],
})
export class EditCollaboratorsDialogComponent {
    @ViewChild('confirmationButton') confirmationButton: MatButton;
    @ViewChild('groupCollaboratorEditor') groupCollaboratorEditor: GroupCollaboratorEditorComponent;

    constructor(public dialogRef: MatDialogRef<EditCollaboratorsDialogComponent>,
                private groupService: GroupService,
                private dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId: string, existingCollaborators: { id: string }[] }) {
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
        await this.groupService.updateGroupCollaboratorList(this.data.groupId, this.groupCollaboratorEditor.getCollaborators());

        // Indicate that the process has ended
        classList.remove('started');
        classList.add('completed');

        this.dialogRef.close({success: true});
    }
}
