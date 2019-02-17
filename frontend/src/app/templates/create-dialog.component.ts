import { Component, Inject } from '@angular/core';
import { ServiceEnableHowTo, ServiceEnableMessage, ServiceEnableEntry, ServiceEnableType } from '../service';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SessionService } from '../session.service';
import { ServiceService } from '../service.service';
import { Template } from './template';

type VariableType = 'input' | 'ouput';

@Component({
    selector: 'template-create-dialog-component',
    templateUrl: './create-dialog.component.html',
    styleUrls: [
        './create-dialog.component.css',
    ],
    providers: [SessionService, ServiceService],
})
export class TemplateCreateDialogComponent {
    template: Template;
    selectedVar: HTMLElement;

    constructor(
        public dialogRef: MatDialogRef<TemplateCreateDialogComponent>,
        public serviceService: ServiceService,
        @Inject(MAT_DIALOG_DATA)
        public data: { template: Template }
    ) {
        this.template = data.template || { name: "", content: "" };

        dialogRef.afterOpen().subscribe(() => {
            const dialogElement = document.getElementById(this.dialogRef.id);
            const editor = dialogElement.querySelector("[name='template-content']");
            (editor as HTMLDivElement).onblur = () => this.onTemplateChange();
            this.onTemplateChange();
        });

    }


    onNoClick(): void {
        this.dialogRef.close();
    }

    getComponentMarker(): string {
        const dialogElement = document.getElementById(this.dialogRef.id);
        const content = dialogElement.getElementsByClassName('mat-dialog-content')[0];
        const attrs = content.attributes;
        for (let i = 0; i < attrs.length; i++) {
            const attr = attrs[i];
            if (attr.name.startsWith('_ng')) {
                return attr.name;
            }
        }
    }

    userSelectedVar(name: string, type: VariableType) {
        const dialogElement = document.getElementById(this.dialogRef.id);
        if (this.selectedVar) {
            dialogElement.removeChild(this.selectedVar);
            this.selectedVar = null;
        }

        this.selectedVar = document.createElement('li');
        this.selectedVar.innerText = name;
        this.selectedVar.classList.add('variable');
        this.selectedVar.classList.add(type + '-variable');
        this.selectedVar.classList.add('follower');
        this.selectedVar.style.position = 'absolute';
        this.selectedVar.style.display = 'none';
        this.selectedVar.setAttribute(this.getComponentMarker(), '');

        dialogElement.appendChild(this.selectedVar);

        dialogElement.onmousemove = ((ev) => {
            if (!this.selectedVar) {
                dialogElement.onmousemove = null;
                return;
            }

            this.selectedVar.style.left = ev.x + 'px';
            this.selectedVar.style.top = ev.y + 'px';
            this.selectedVar.style.display = 'block';
        });

        dialogElement.onmouseup = ((ev) => {
            if (!this.selectedVar) {
                dialogElement.onmouseup = null;
                return;
            }

            // Remains dropping the variable references
            dialogElement.removeChild(this.selectedVar);
            this.selectedVar = null;
        });
    }

    onTemplateChange() {
        const dialogElement = document.getElementById(this.dialogRef.id);
        const editor = dialogElement.querySelector("[name='template-content']");
        const marker = this.getComponentMarker();

        const newChilds = [];
        for (let i = 0; i < editor.childNodes.length; i++) {
            const node = editor.childNodes[i];
            if ((node.nodeName !== '#text') && (node.nodeName !== 'SPAN')) {
                // Non text node
                newChilds.push(node);
                continue;
            }

            let content = "";
            if (node.nodeName === '#text') {
                content = (node as any).data as string;
            }
            else if (node.nodeName === 'SPAN') {
                content = (node as HTMLSpanElement).innerText;
            }

            if (content.length === 0) {
                // No content, remove element
                continue;
            }
            if ((content.indexOf(" ") === -1) && (content.indexOf("\n") === -1)) {
                // Node already splitted
                newChilds.push(node);
                continue;
            }

            let remainingContent = content;
            console.log("C:", content);
            while (remainingContent) {
                console.log("Rem:", remainingContent);
                const point = remainingContent.indexOf(' ');
                let text: string;
                let space: string;
                if (point === -1) {
                    text = remainingContent;
                    space = null;
                    remainingContent = null;
                }
                else {
                    text = remainingContent.substr(0, point);
                    space = remainingContent.charAt(point);
                    remainingContent = remainingContent.substr(point + 1);
                }

                if (text) {
                    const newChunk = document.createElement('span');
                    newChunk.innerText = text;
                    newChunk.setAttribute(marker, '');
                    newChilds.push(newChunk);
                }

                if (space) {
                    const spaceElement = document.createElement('span');
                    spaceElement.innerText = space;
                    spaceElement.setAttribute(marker, '');
                    newChilds.push(spaceElement);
                }
            }
        }

        for (let i = 0; i < editor.childNodes.length; i++) {
            const node = editor.childNodes[i];
            editor.removeChild(node);
        }

        for (const node of newChilds) {
            editor.appendChild(node);
        }
    }

}
