import { Component, Input, OnInit } from '@angular/core';
import { ProgramContent, ProgramInfoUpdate, ProgramLogEntry } from 'app/program';
import { ProgramService } from 'app/program.service';
import { Session } from 'app/session';
import { SessionService } from 'app/session.service';
import { unixMsToStr } from 'app/utils';
import { ToastrService } from 'ngx-toastr';
import { Unsubscribable } from 'rxjs';
import { environment } from 'environments/environment';
import { EnvironmentDefinition } from 'environments/environment-definition';

type DrawerType = 'logs' | 'variables' | 'custom';

@Component({
    selector: 'app-program-editor-sidepanel',
    templateUrl: './program-editor-sidepanel.component.html',
    styleUrls: [
        './program-editor-sidepanel.component.scss',
        '../../libs/css/material-icons.css',
    ],
    providers: [SessionService, ProgramService, ToastrService],
})
export class ProgramEditorSidepanelComponent implements OnInit {
    // Inputs
    @Input() program: ProgramContent;
    @Input() onToggleMark: (blockId: string, activate: boolean, message: string) => void;
    @Input() onClose: () => void;
    @Input() noVariables: boolean;
    @Input() customElementName: string  | null;

    // Utils for template
    readonly _typeof = (x: any) => typeof x;
    readonly json_stringify = JSON.stringify;

    // State data
    session: Session;
    drawer_type: DrawerType | null = null;
    commented_blocks: { [key:string]: [number, HTMLButtonElement]} = {};
    drawer_initialized: boolean = false;
    environment: EnvironmentDefinition;

    constructor(
        private sessionService: SessionService,
        private programService: ProgramService,
        private toastr: ToastrService,
    ) {
        this.environment = environment;
    }

    async ngOnInit(): Promise<void> {
        this.session = await this.sessionService.getSession();
        if (!this.program.readonly) {
            this.streamingLogs = true;
            this.logSubscription = this.programService.watchProgramLogs(this.program.id,
                                                                        { request_previous_logs: true })
                .subscribe(
                    {
                        next: (update: ProgramInfoUpdate) => {
                            if (update.value.program_id !== this.program.id) {
                                return;
                            }

                            if (update.type === 'program_log') {
                                this.updateLogsDrawer(update.value);
                                this.logCount++;
                            }
                            else if (update.type === 'debug_log') {
                                this.updateLogsDrawer(update.value);
                                this.logCount++;
                            }
                        },
                        error: (error: any) => {
                            console.error("Error reading logs:", error);
                            this.streamingLogs = false;
                        },
                        complete: () => {
                            console.warn("No more logs about program", this.program.id)
                            this.streamingLogs = false;
                        }
                    });

            this.variableSubscription = this.programService.watchProgramVariables(this.program.id,
                                                                                  { request_previous: true })
                .subscribe(
                    {
                        next: (variable: { name: string, value: any }) => {
                            const idx = this.variables.findIndex(v => v.name === variable.name);

                            if (idx === -1) {
                                this.variables.push({ name: variable.name, value: variable.value });
                            }
                            else {
                                if ((!this.serverVariables[variable.name])
                                    || (this.serverVariables[variable.name] === this.variables[idx].value)) {

                                    this.variables[idx].value = variable.value;
                                }
                            }

                            this.serverVariables[variable.name] = variable.value;
                            this.updateVariable(variable.name);
                        },
                        error: (error: any) => {
                            console.error("Error reading variables:", error);
                            this.streamingVariables = false;
                        },
                        complete: () => {
                            console.warn("No more variable updates on program", this.program.id)
                            this.streamingVariables = false;
                        }
                    });

        }
    }

    public setDrawerType(type: DrawerType) {
        this.drawer_type = type;
    }

    get drawerType() : DrawerType | null {
        return this.drawer_type;
    }

    public dispose() {
        if (this.logSubscription) {
            this.logSubscription.unsubscribe();
            this.logSubscription = null;
        }

        if (this.variableSubscription) {
            this.variableSubscription.unsubscribe();
            this.variableSubscription = null;
        }
    }

    // Log management
    streamingLogs = false;
    logCount = 0;
    logSubscription: Unsubscribable | null;

    updateLogsDrawer(line: ProgramLogEntry) {
        const container = document.getElementById('logs_panel_container');
        if (!this.drawer_initialized) {
            container.innerHTML = ''; // Clear container

            this.drawer_initialized = true;
        }

        const newLine = this.renderLogLine(line);
        container.appendChild(newLine);

        if (this.drawer_type === 'logs') {
            newLine.scrollIntoView();
        }
    }

    renderLogLine(line: ProgramLogEntry): HTMLElement {
        const element = document.createElement('div');
        element.classList.add('log-entry');

        const line_time = document.createElement('span');
        line_time.classList.add('time');
        line_time.innerText = unixMsToStr(line.event_time);

        element.appendChild(line_time);

        const message = document.createElement('span');
        message.classList.add('message');
        message.innerText = line.event_message;

        element.appendChild(message);

        if (line.block_id && this.onToggleMark) {
            const mark_button = document.createElement('button');
            mark_button.classList.value = 'log-marker mat-button mat-raised-button mat-button-base mat-primary';

            mark_button.innerText = 'Mark block';
            mark_button.onclick = () => {
                this.toggleMark(mark_button, line);
            }

            element.appendChild(mark_button);
        }

        return element;
    }

    toggleMark(button: HTMLButtonElement, log_line: ProgramLogEntry) {
        const entry = this.commented_blocks[log_line.block_id];
        const marked = (!entry) || (entry[0] !== log_line.event_time);
        this.onToggleMark(log_line.block_id, marked, log_line.event_message);

        if (!marked) {
            this.commented_blocks[log_line.block_id] = undefined;
            button.innerText = 'Mark block';
        }
        else {
            button.innerText = 'Unmark block';
            if (entry !== undefined) {
                entry[1].innerText = 'Mark block';
            }

            this.commented_blocks[log_line.block_id] = [log_line.event_time, button];
        }
    }

    // Variable management
    variableSubscription: Unsubscribable | null;
    variables: { name: string, value: any }[] = [];
    serverVariables: { [key: string]: any } = {};
    streamingVariables = false;
    updatedVariables: string[] = [];
    variablesInCreation: { name: string, value: any }[] = [];
    variablesToRemove: string[] = [];


    preparedChangeOnVar(name: string): boolean {
        const idx = this.variables.findIndex(v => v.name === name);

        if (idx < 0) {
            return false;
        }
        else if (!this.serverVariables[name]) {
            return true;
        }
        else {
            return this.variables[idx].value !== this.serverVariables[name];
        }
    }

    resetVariable(name: string) {
        const idx = this.variables.findIndex(v => v.name === name);

        if (!this.serverVariables[name]) {
            if (idx >= 0) {
                this.variables.splice(idx, 1);
            }
        }
        else {
            this.variables[idx].value = this.serverVariables[name];
        }

        this.updateVariable(name);
    }

    updateVariable(name: string) {
        const val = this.variables.find(v => v.name === name).value;
        const indexInUpdate = this.updatedVariables.indexOf(name);

        if (val !== this.serverVariables[name]) {
            if (indexInUpdate < 0) {
                this.updatedVariables.push(name);
            }
        }
        else {
            if (indexInUpdate >= 0) {
                this.updatedVariables.splice(indexInUpdate, 1);
            }
        }
    }

    addVariable() {
        let index = 1;
        for (; this.variables.find(v => v.name === 'var-' + index)
            || this.variablesInCreation.find(v => v.name === 'var-' + index)
            ; index++ ) {}
        this.variablesInCreation.push({ name: 'var-' + index, value: "Value" });
    }

    removeVariable(name: string) {
        this.variablesToRemove.push(name);
    }

    removeVariableInCreation(name: string) {
        const idx = this.variablesInCreation.findIndex(v => v.name === name);
        this.variablesInCreation.splice(idx, 1);
    }

    unremoveVariable(name: string) {
        const idx = this.variablesToRemove.indexOf(name);
        this.variablesToRemove.splice(idx, 1);
    }

    async uploadVariableChanges() {
        const target: { name: string, value: any }[] = [];
        for (const name of this.updatedVariables) {
            const result = this.variables.find(v => v.name === name);
            target.push({ name: name, value: result.value });
        }
        for (const item of this.variablesInCreation) {
            target.push({ name: item.name, value: item.value });
        }

        try {
            await this.programService.updateProgramVariables(this.program.id, target);
            this.variablesInCreation = [];

            await Promise.all(this.variablesToRemove.map(name => this.programService.removeVariable(this.program.id, name)));

            for (const name of this.variablesToRemove) {
                const idx = this.variables.findIndex(v => v.name === name);
                delete this.serverVariables[name];
                if (idx >= 0) {
                    this.variables.splice(idx, 1);
                }
            }
            this.variablesToRemove = [];

            this.toastr.success('Change complete', '', {
                closeButton: true,
                progressBar: true,
            });
        }
        catch (error) {
            this.toastr.error(JSON.stringify(error), 'Error on update', {
                closeButton: true,
                progressBar: true,
            });

            console.error(error);
        }
    }
}
