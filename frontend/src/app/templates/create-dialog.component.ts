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
        this.selectedVar.setAttribute('var-type', type);
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

            this.selectedVar.style.left = (ev.x + 2) + 'px';
            this.selectedVar.style.top = (ev.y + 2) + 'px';
            this.selectedVar.style.display = 'block';
        });

        dialogElement.onmouseup = ((ev) => {
            if (!this.selectedVar) {
                dialogElement.onmouseup = null;
                return;
            }

            let element = document.elementFromPoint(ev.x, ev.y) as HTMLElement;
            if (this.elementInEditor(element)) {
                const rect = element.getBoundingClientRect();
                const middle = rect.left + rect.width / 2;
                let after = ev.x > middle;

                const newElement = this.buildBadgeFrom(this.selectedVar as HTMLElement);
                let parent = element.parentNode;
                if (this.isEditor(element)) {
                    element.appendChild(newElement);
                }
                else {
                    if (this.isVariable(element)) {
                        element = this.variableParent(element);
                        parent = element.parentNode;
                    }
                    if (after) {
                        if (!element.nextSibling) {
                            parent.appendChild(newElement);
                        }
                        parent.insertBefore(newElement, element.nextSibling);
                    }
                    else {
                        parent.insertBefore(newElement, element);
                    }
                }
            }

            // Remains dropping the variable references
            dialogElement.removeChild(this.selectedVar);
            this.selectedVar = null;
        });
    }

    variableParent(element: HTMLElement): HTMLElement {
        let node = element as HTMLElement;
        while (node) {
            if (node.classList.contains("variable")) {
                return node as HTMLElement;
            }

            node = node.parentNode as HTMLElement;
        }
        return element;
    }

    isVariable(element: HTMLElement): boolean {
        const dialogElement = document.getElementById(this.dialogRef.id);
        const editor = dialogElement.querySelector("[name='template-content']");

        let node = element as HTMLElement;
        while (node) {
            if (node.classList.contains("variable")) {
                return true;
            }

            if ((node === dialogElement) || (node === editor)) {
                return false;
            }

            node = node.parentNode as HTMLElement;
        }

        return false;
    }

    buildBadgeFrom(selectedVar: HTMLElement) {
        const marker = this.getComponentMarker();

        const element = document.createElement('span');
        (selectedVar.classList as any as Array<string>).forEach((value, _k, _p) => {
            element.classList.add(value);
        });
        element.setAttribute('contenteditable', 'false');
        element.setAttribute(marker, '');
        element.classList.add('badge');
        element.setAttribute('var-type', selectedVar.getAttribute('var-type'));

        const text = document.createElement('span');
        text.setAttribute(marker, '');
        text.classList.add('text');
        text.innerText = selectedVar.innerText;
        element.appendChild(text);

        const remover = document.createElement('span');
        remover.innerText = 'X';
        remover.setAttribute(marker, '');
        remover.classList.add('remover');
        remover.onclick = (ev) => {
            element.remove();
            ev.stopPropagation();
        }
        element.appendChild(remover);

        text.onmousedown = () => {
            this.userSelectedVar(text.innerText, element.getAttribute('var-type') as VariableType);
            element.remove();
        }

        return element;
    }

    isEditor(element: Element): boolean {
        return element.getAttribute('name') === 'template-content';
    }

    elementInEditor(element: Element): boolean {
        const dialogElement = document.getElementById(this.dialogRef.id);
        const editor = dialogElement.querySelector("[name='template-content']");

        let node = element as HTMLElement;
        while (node) {
            if (node === editor) {
                return true;
            }

            if (node === dialogElement) {
                return false;
            }

            node = node.parentNode as HTMLElement;
        }

        return false;
    }

    onTemplateChange() {
        const dialogElement = document.getElementById(this.dialogRef.id);
        const editor = dialogElement.querySelector("[name='template-content']");
        const marker = this.getComponentMarker();

        this.splitChildren(editor as HTMLElement, marker);
    }

    splitChildren(element: HTMLElement, marker: string) {
        const newChilds = [];
        for (let i = 0; i < element.childNodes.length; i++) {
            const node = element.childNodes[i];

            if (node.nodeName === 'DIV') {
                this.splitChildren(node as HTMLDivElement, marker);
                newChilds.push(node);
                continue;
            }

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
            while (remainingContent) {
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

        while (element.firstChild) {
            (element.firstChild as HTMLElement).remove();
        }

        for (const node of newChilds) {
            element.appendChild(node);
        }
    }

}
