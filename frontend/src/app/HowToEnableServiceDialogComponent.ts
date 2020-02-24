import { Component, Inject } from '@angular/core';
import { ServiceEnableHowTo, ServiceEnableMessage, ServiceEnableEntry, ServiceEnableType } from './service';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SessionService } from './session.service';
import { ServiceService } from './service.service';

@Component({
    selector: 'app-how-to-enable-service-dialog',
    templateUrl: 'how-to-enable-service-dialog.html',
    styleUrls: [
        'how-to-enable-service-dialog.css',
    ],
    providers: [SessionService, ServiceService],
})

export class HowToEnableServiceDialogComponent {
    form = {};
    service: ServiceEnableHowTo;
    renderingZone: HTMLDivElement;
    type: ServiceEnableType;

    constructor(
        public dialogRef: MatDialogRef<HowToEnableServiceDialogComponent>,
        public serviceService: ServiceService,
        @Inject(MAT_DIALOG_DATA)
        public data: ServiceEnableHowTo
    ) {
        this.service = data;

        dialogRef.afterOpened().subscribe(() => {
            this.renderingZone = (document
                .getElementById(dialogRef.id)
                .getElementsByClassName("rendering-zone")[0]) as HTMLDivElement;

            this.renderingZone.appendChild(this.render(data));
        });
    }

    render(data: ServiceEnableHowTo): HTMLElement {
        this.type = data.type;

        if (data.type === 'message') {
            return this.render_scripted_form(data as ServiceEnableMessage);
        }

        else if (data.type === 'form') {
            return this.render_scripted_form(data as ServiceEnableMessage);
        }

        throw new Error("Cannot render type: " + data.type);
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
            let element;
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
                        this.input_controls_field(element, entry.properties.name);
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

    send_form(): void {
        this.serviceService.registerService(this.form,
                                            this.data.metadata.service_id,
                                            this.data.metadata.connection_id)
            .then((result) => {
                if (result.success) {
                    this.dialogRef.close();
                }
            }).catch((error) => {
                console.error("Error registering", error);
            });
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
