import { Component, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { SessionService } from '../session.service';
import { ServiceService } from '../service.service';
import { Template } from './template';

type VariableType = 'input' | 'ouput';
type PromiseHandler = { resolve: (value: [string, any[]]) => void, reject: Function };
type TemplateChunk = { type: 'text' | 'line' | 'variable', content: string | TemplateChunk[], class?: string };

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
    promise: PromiseHandler;
    selectedVar: HTMLElement;
    variables: string[];
    usedOutputs: {[key: string]: boolean};
    allOutputsUsed: boolean;

    constructor(
        public dialogRef: MatDialogRef<TemplateCreateDialogComponent>,
        public serviceService: ServiceService,
        @Inject(MAT_DIALOG_DATA)
        public data: { template: Template, promise: PromiseHandler, variables: [string] }
    ) {
        this.template = data.template || { id: null, name: "", content: [] };
        this.promise = data.promise;
        this.variables = data.variables;
        this.usedOutputs = {};
        this.checkAllOutputsUsed();

        dialogRef.afterOpened().subscribe(() => {
            const dialogElement = document.getElementById(this.dialogRef.id);
            const editor = dialogElement.querySelector("[name='template-content']");
            (editor as HTMLDivElement).onblur = () => this.onTemplateChange();
            this.onTemplateChange();
        });

    }

    checkAllOutputsUsed() {
        let available = this.variables.length;
        for (let name of this.variables) {
            if (this.usedOutputs[name]) {
                available--;
            }
        }

        this.allOutputsUsed = available === 0;
    }


    onNoClick(): void {
        this.dialogRef.close();
        this.promise.reject();
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
        const variableName = selectedVar.innerText;
        const varType = selectedVar.getAttribute('var-type');

        const element = document.createElement('span');
        (selectedVar.classList as any as Array<string>).forEach((value, _k, _p) => {
            element.classList.add(value);
        });
        element.setAttribute('contenteditable', 'false');
        element.setAttribute(marker, '');
        element.classList.add('badge');
        element.setAttribute('var-type', varType);

        const text = document.createElement('span');
        text.setAttribute(marker, '');
        text.classList.add('text');
        text.innerText = variableName;
        element.appendChild(text);

        const remover = document.createElement('span');
        remover.innerText = 'X';
        remover.setAttribute(marker, '');
        remover.classList.add('remover');
        remover.onclick = (ev) => {
            element.remove();
            ev.stopPropagation();

            if (varType === 'output') {
                this.setOutputUsage(variableName, false);
            }
        }
        element.appendChild(remover);

        text.onmousedown = () => {
            this.userSelectedVar(text.innerText, element.getAttribute('var-type') as VariableType);
            element.remove();
            if (varType === 'output') {
                this.setOutputUsage(variableName, false);
            }
        }

        if (varType === 'output') {
            this.setOutputUsage(variableName, true);
        }

        return element;
    }

    setOutputUsage(name: string, value: boolean) {
        this.usedOutputs[name] = value;

        this.checkAllOutputsUsed();
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

    extractTemplate(element: HTMLElement): TemplateChunk[] {
        const children: TemplateChunk[] = [];
        for (let i = 0; i < element.childNodes.length; i++) {
            const node = element.childNodes[i];

            // Raw text
            if (node.nodeName === '#text') {
                children.push({
                    "type": "text",
                    "content": (node as any).data as string,
                });
                continue;
            }
            // Line
            else if (node.nodeName === 'DIV') {
                const content = this.extractTemplate(node as HTMLDivElement);
                children.push({ "type": "line", "content": content });
                continue;
            }

            else if (node.nodeName !== 'SPAN') {
                console.error("Unknown node type:", node.nodeName);
                continue;
            }

            // Line tag
            else if (!(node as HTMLElement).classList.contains('variable')) {
                children.push({
                    "type": "text",
                    "content": (node as HTMLElement).innerText as string,
                });
                continue;
            }

            // Variable badge
            const text = (node as HTMLElement).getElementsByClassName('text')[0] as HTMLElement;
            let nodeClass = null;
            ((node as HTMLElement).classList as any as Array<string>).forEach((v, _k, _p) => {
                const suffix = '-variable';
                if (v.endsWith(suffix)) {
                    nodeClass = v.substr(0, v.length - suffix.length);
                }
            });

            children.push({
                "type": "variable",
                "class": nodeClass,
                "content": text.innerText,
            })
        }

        return children;
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
                const spanNode = node as HTMLSpanElement;

                if (spanNode.classList.contains('badge')) {
                    // Non-splittable element
                    newChilds.push(node);
                    continue;
                }
                else {
                    content = spanNode.innerText;
                }
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

    send_form() {
        this.onTemplateChange();

        const dialogElement = document.getElementById(this.dialogRef.id);
        const editor = dialogElement.querySelector("[name='template-content']");
        const value = this.extractTemplate(editor as HTMLElement);
        this.template.content = value;

        this.dialogRef.close();
        this.promise.resolve([this.template.name, this.template.content]);
    }

}
