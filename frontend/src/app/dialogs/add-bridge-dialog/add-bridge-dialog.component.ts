import { Component, Inject, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatButton } from '@angular/material/button';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeMetadata } from 'app/bridges/bridge';
import { BridgeService } from 'app/bridges/bridge.service';
import { GroupService } from 'app/group.service';
import { SessionService } from 'app/session.service';

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
    bridgeId: string | null;
    bridgeCreated = false;

    options: FormGroup;

    constructor(public dialogRef: MatDialogRef<AddBridgeDialogComponent>,
                private bridgeService: BridgeService,
                private formBuilder: FormBuilder,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId?: string }) {
    }

    async ngOnInit() {
        this.options = this.formBuilder.group({
            bridgeName: ['', [Validators.required, Validators.minLength(4)]],
            bridgeControlUrl: ['', []],
        });
    }

    onBack(): void {
        this.dialogRef.close({success: false, id: null, name: null});
    }

    create(): void {
        const bridgeName = this.options.controls.bridgeName.value;

        // Indicate that the process has started
        const classList = this.confirmationButton._elementRef.nativeElement.classList;
        classList.add('started');
        classList.remove('completed');

        this.bridgeCreated = true;

        let createGroupProcess: Promise<BridgeMetadata>;
        if (this.data.groupId) {
            createGroupProcess = this.bridgeService.createGroupBridge(bridgeName, this.data.groupId);
        }
        else {
            createGroupProcess = this.bridgeService.createServicePort(bridgeName);
        }

        createGroupProcess.then((BridgeMetadata: BridgeMetadata) => {
            this.dialogRef.close({success: this.bridgeCreated, bridgeId: BridgeMetadata.id, bridgeName: bridgeName});
        }).catch(() => {
            this.bridgeCreated = false;
        }).then(() => {
            // Indicate that the process has ended
            classList.remove('started');
            classList.add('completed');
        });

    }

}
