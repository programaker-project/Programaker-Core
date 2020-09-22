import { Component, Inject, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatButton } from '@angular/material/button';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeMetadata } from 'app/bridges/bridge';
import { BridgeService } from 'app/bridges/bridge.service';
import { GroupService } from 'app/group.service';
import { SessionService } from 'app/session.service';
import { toWebsocketUrl } from 'app/utils';

@Component({
    selector: 'app-add-bridge-dialog',
    templateUrl: 'add-bridge-dialog.component.html',
    styleUrls: [
        'add-bridge-dialog.component.css',
        '../../libs/css/material-icons.css',
    ],
    providers: [BridgeService, SessionService, GroupService],
})
export class AddBridgeDialogComponent {
    @ViewChild('confirmationButton') confirmationButton: MatButton;
    bridgeControlUrl = "";
    bridgeCreated = false;

    options: FormGroup;

    constructor(public dialogRef: MatDialogRef<AddBridgeDialogComponent>,
                private bridgeService: BridgeService,
                private formBuilder: FormBuilder,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId: string }) {
    }

    async ngOnInit() {
        this.options = this.formBuilder.group({
            bridgeName: ['', [Validators.required, Validators.minLength(4)]],
            bridgeControlUrl: ['', []],
        });
    }

    onBack(): void {
        this.dialogRef.close({success: this.bridgeCreated});
    }

    create(): void {
        const bridgeName = this.options.controls.bridgeName.value;

        // Indicate that the process has started
        const classList = this.confirmationButton._elementRef.nativeElement.classList;
        classList.add('started');
        classList.remove('completed');

        this.bridgeCreated = true;
        this.bridgeService.createGroupBridge(bridgeName, this.data.groupId).then((BridgeMetadata: BridgeMetadata) => {
            this.bridgeControlUrl = toWebsocketUrl(BridgeMetadata.control_url);

            this.options.controls.bridgeName.disable();
        }).catch(() => {
            this.bridgeCreated = false;
        }).then(() => {
            // Indicate that the process has ended
            classList.remove('started');
            classList.add('completed');
        });

    }

}
