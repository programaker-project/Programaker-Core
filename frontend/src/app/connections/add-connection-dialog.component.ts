import { Component, Inject, ViewChild, ElementRef } from '@angular/core';
import { SessionService } from '../session.service';
import { ConnectionService } from '../connection.service';
import { ServiceService } from '../service.service';
import { BridgeService } from '../bridges/bridge.service';

import { ServiceEnableHowTo, ServiceEnableMessage, ServiceEnableEntry, TwoStepEnableMetadata } from '../service';

import { MatDialog } from '@angular/material/dialog';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { BridgeIndexData } from '../bridges/bridge';

@Component({
    selector: 'app-add-connection-dialog',
    templateUrl: 'add-connection-dialog.component.html',
    styleUrls: [
        'add-connection-dialog.component.css',
        '../libs/css/material-icons.css',
    ],
    providers: [BridgeService, SessionService, ServiceService, ConnectionService],
})
export class AddConnectionDialogComponent {
    howToEnable: ServiceEnableHowTo;
    form = {};

    errorType: null | 'bridge_not_ready';
    error: any;

    @ViewChild('renderingZone') renderingZone: ElementRef<HTMLDivElement>;
    type: string = '';
    ready = false;

    constructor(private dialogRef: MatDialogRef<AddConnectionDialogComponent>,
                private sessionService: SessionService,
                private serviceService: ServiceService,
                private connectionService: ConnectionService,
                private dialog: MatDialog,

                @Inject(MAT_DIALOG_DATA)
                public data: { groupId?: string, programId?: string, bridgeInfo: BridgeIndexData }) {
    }

    onNoClick(): void {
        this.dialogRef.close({success: false});
    }

    async ngOnInit() {
        const as_service = await this.connectionService.toAvailableService(this.data.bridgeInfo);

        let query: Promise<ServiceEnableHowTo>;
        if (this.data.programId) {
            query = this.serviceService.getHowToEnableOnProgram(as_service, this.data.programId);
        }
        else if (this.data.groupId) {
            query = this.serviceService.getHowToEnableOnGroup(as_service, this.data.groupId);
        }
        else {
            query = this.serviceService.getHowToEnable(as_service)
        }

        try {
            this.howToEnable = await query;
        }
        catch (error) {
            this.error = error;
            if ((error.name === 'HttpErrorResponse') && (error.status === 409)) {
                // User cannot add connections, so skip adding them
                this.errorType = 'bridge_not_ready';
            }
            return;
        }
        console.log("How to enable", this.howToEnable);
        if (this.howToEnable.type === 'direct') {
            this.ready = true;
            return;
        }

        if (this.howToEnable.type === 'message') {
            // Open websocket to monitor for side-channel connection
            (this.connectionService.waitForPendingConnectionEstablished(this.howToEnable.metadata.connection_id)
                .then(_success => {
                    this.dialogRef.close({ success: true });
                }))
        }

        this.render(this.howToEnable as ServiceEnableMessage);
        this.ready = true;
    }

    establishConnection(): void {
        if (this.howToEnable.type === 'direct') {
            let query: Promise<{success: boolean}>;
            if (this.data.programId) {
                query = this.serviceService.directRegisterServiceOnProgram(this.data.bridgeInfo.id, this.data.programId)
            }
            else if (this.data.groupId) {
                query = this.serviceService.directRegisterServiceOnGroup(this.data.bridgeInfo.id, this.data.groupId)
            }
            else {
                query = this.serviceService.directRegisterService(this.data.bridgeInfo.id)
            }

            query.then((result) => {
                console.log("Result:", result);
                if (result.success) {
                    this.dialogRef.close({success: true});
                }
            }).catch((error) => {
                console.error("Error registering", error);
            });
        }
        else if (this.howToEnable.type === 'form') {
            this.sendForm();
        }
    }

    render(data: ServiceEnableMessage) {
        this.type = data.type;

        let element: HTMLDivElement;
        if (data.type === 'message') {
            element = this.render_scripted_form(data as ServiceEnableMessage);
        }

        else if (data.type === 'form') {
            element = this.render_scripted_form(data as ServiceEnableMessage);
        }
        else {
            this.error = 'Cannot render ' + data.type;
        }

        const zone = this.renderingZone.nativeElement;
        while (zone.lastElementChild) {
            zone.removeChild(zone.lastElementChild);
        }
        zone.appendChild(element);
    }

    render_scripted_form(data: ServiceEnableMessage): HTMLDivElement {
        const topMost = document.createElement("div");

        for (const entry of data.value.form) {
            topMost.appendChild(this.render_form_entry(entry));
        }

        return topMost;
    }

    render_form_entry(entry: ServiceEnableEntry): HTMLElement | Text {
        if (entry.type === 'text') {
            const element = document.createElement('span');
            element.classList.add('text');
            element.innerText = entry.value;
            return element;
        }
        else if (entry.type === 'console') {
            const element = document.createElement('div');
            element.classList.add('console');
            element.innerText = entry.value;
            return element;
        }
        else if (entry.type === 'tag') {
            let element: HTMLElement;
            if (entry.tag === 'u') {
                element = document.createElement('u');
            }
            else if (entry.tag === 'console') {
                element = document.createElement('div');
                element.classList.add('console');
            }
            else if (entry.tag === 'a') {
                element = document.createElement('a');
                if ((entry.properties !== undefined) && (entry.properties.href !== undefined)) {
                    element.setAttribute('href', entry.properties.href);
                }

                element.setAttribute('target', '_blank');
                element.setAttribute('rel', 'noopener noreferer');

            }
            else if (entry.tag === 'autolink') {
                element = document.createElement('a');
                /// @TODO: Complete functionality
            }
            else if (entry.tag === 'value') {
                element = document.createElement('span');
                /// @TODO: Complete functionality
                if (entry.properties !== undefined) {
                    element.innerText = entry.properties.placeholder || '';
                }
            }
            else if (entry.tag === 'input') {
                element = document.createElement('input');
                if (entry.properties !== undefined) {
                    const allowedProperties = ['type', 'placeholder', 'value', 'name'];

                    for (const property of allowedProperties) {
                        if (entry.properties[property] !== undefined) {
                            element.setAttribute(property, entry.properties[property]);
                        }
                    }

                    if (entry.properties.name) {
                        this.input_controls_field(element as HTMLInputElement, entry.properties.name);
                    }
                }
            }
            else {
                throw new Error("Unknown tag: " + entry.tag);
            }
            for (const child of entry.content) {
                element.appendChild(this.render_form_entry(child));
            }

            return element;
        }
    }

    input_controls_field(entry: HTMLInputElement, fieldName: string) {
        const update_value = () => {
            this.form[fieldName] = entry.value;
        }

        entry.onchange = update_value;
        update_value();
    }

    sendForm(): void {
        this.serviceService.registerService(this.form,
                                            this.data.bridgeInfo.id,
                                            (this.howToEnable.metadata as TwoStepEnableMetadata).connection_id)
            .then((result) => {
                if (result.success) {
                    this.dialogRef.close({ success: true });
                }
            }).catch((error) => {
                console.error("Error registering", error);
            });
    }
}
