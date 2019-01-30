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

        dialogRef.afterOpen().subscribe(() => {
            this.renderingZone = (document
                .getElementById(dialogRef.id)
                .getElementsByClassName("rendering-zone")[0]) as HTMLDivElement;

            this.renderingZone.appendChild(this.render(data));
        });
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

    render_form_entry(entry: ServiceEnableFormEntry): HTMLElement | Text {
        if (entry.type === 'text') {
            const element = document.createTextNode(entry.value);
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
            else if (entry.tag === 'a') {
                element = document.createElement('a');
                if ((entry.properties !== undefined) && (entry.properties.href !== undefined)) {
                    element.setAttribute('href', entry.properties.href);
                }
            }
            else {
                throw new Error("Unknown tag: "+ entry.tag);
            }
            for (const child of entry.content) {
                element.appendChild(this.render_form_entry(child));
            }

            return element;
        }
    }

    onNoClick(): void {
        this.dialogRef.close();
    }
}
