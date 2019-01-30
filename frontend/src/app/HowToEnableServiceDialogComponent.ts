import { Component, Inject } from '@angular/core';
import { ServiceEnableHowTo, ServiceEnableScriptedForm, ServiceEnableFormEntry } from './service';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
@Component({
    selector: 'app-how-to-enable-service-dialog',
    templateUrl: 'how-to-enable-service-dialog.html',
    styleUrls: [
        'how-to-enable-service-dialog.css',
    ]
})

export class HowToEnableServiceDialogComponent {
    service: ServiceEnableHowTo;
    renderingZone: HTMLDivElement;

    constructor(public dialogRef: MatDialogRef<HowToEnableServiceDialogComponent>,
        @Inject(MAT_DIALOG_DATA)
        public data: ServiceEnableHowTo) {
        this.service = data;

        setTimeout(() => {
            this.renderingZone = (document
                .getElementById(dialogRef.id)
                .getElementsByClassName("rendering-zone")[0]) as HTMLDivElement;

            this.renderingZone.appendChild(this.render(data));
        }, 1000);
    }

    render(data: ServiceEnableHowTo): HTMLElement {
        if (data.type === 'scripted-form') {
            return this.render_scripted_form(data as ServiceEnableScriptedForm);
        }

        throw new Error("Cannot render type: " + data.type);
    }

    render_scripted_form(data: ServiceEnableScriptedForm): HTMLDivElement {
        const topMost = document.createElement("div");

        for (const entry of data.value.form) {
            topMost.appendChild(this.render_form_entry(entry));
        }

        return topMost;
    }

    render_form_entry(entry: ServiceEnableFormEntry): HTMLDivElement {
        const element = document.createElement('div');
        if (entry.type === 'text') {
            // Nothing to do
        }
        else if (entry.type === 'console') {
            element.classList.add('console');
        }

        element.innerText = entry.value;
        return element;
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
