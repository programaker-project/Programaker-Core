import { map, share} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { ProgramMetadata, ProgramContent, ProgramInfoUpdate, ProgramLogEntry, ProgramType, ProgramEditorEvent, ProgramEditorEventValue } from './program';

import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';
import { toWebsocketUrl, addTokenQueryString } from './utils';
import { Synchronizer } from './syncronizer';
import { EnvironmentService } from './environment.service';
import { ImageAssetConfiguration } from './flow-editor/dialogs/configure-block-dialog/configure-block-dialog.component';

@Injectable()
export class ProgramService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getListProgramsUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/programs/';
    }

    async getCreateProgramsUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/programs/';
    }

    private getGroupCreateProgramsUrl(groupId: string) {
        return `${this.environmentService.getApiRoot()}/groups/by-id/${groupId}/programs`;
    }

    private listProgramsOnGroupUrl(groupId: string) {
        return `${this.environmentService.getApiRoot()}/groups/by-id/${groupId}/programs`;
    }

    private getRetrieveProgramUrl(userId: string, programName: string) {
        return `${this.environmentService.getApiRoot()}/users/${userId}/programs/${programName}`;
    }

    private getRetrieveProgramUrlById(programId: string): string {
        return this.environmentService.getApiRoot() + '/programs/by-id/' + programId;
    }

    async getUpdateProgramUrl(programId: string) {
        return this.environmentService.getApiRoot() + '/programs/by-id/' + programId;
    }

    async getProgramCheckpointUrlById(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/checkpoint`;
    }

    async getProgramTagsUrl(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/tags`;
    }

    private async getProgramLogsUrl(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/logs`;
    }

    private async getProgramStreamingLogsUrl(programId: string) {
        const token = this.sessionService.getToken();
        return addTokenQueryString(toWebsocketUrl(this.environmentService,
                                                  `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/logs-stream`),
                                  token,
                                 );
    }

    private async getProgramStreamingEventsUrl(programId: string) {
        const token = this.sessionService.getToken();
        return addTokenQueryString(toWebsocketUrl(this.environmentService,
                                                  `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/editor-events`),
                                   token,
                                  );
    }

    async getProgramStopThreadsUrl(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/stop-threads`;
    }

    async getProgramsStatusUrl(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/status`;
    }
    getAssetUrlOnProgram(assetId: string, programId: string): string {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/assets/by-id/${assetId}`;
    }

    getPrograms(): Promise<ProgramMetadata[]> {
        return this.getListProgramsUrl().then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                map(response => response as ProgramMetadata[]))
                .toPromise());
    }

    async getProgramsOnGroup(groupId: string): Promise<ProgramMetadata[]> {
        const url = this.listProgramsOnGroupUrl(groupId)

        const result = await this.http.get(url, {headers: this.sessionService.getAuthHeader()}).toPromise();

        return result['programs'];
    }

    async getProgram(userName: string, programName: string): Promise<ProgramContent> {
        const url = this.getRetrieveProgramUrl(userName, programName)

        const result = await this.http.get(url, {headers: this.sessionService.getAuthHeader()}).toPromise();

        return result as ProgramContent;
    }

    async getProgramById(programId: string): Promise<ProgramContent> {
        const url = this.getRetrieveProgramUrlById(programId);
        return (this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                .toPromise() as Promise<ProgramContent>);
    }

    getProgramTags(programId: string): Promise<string[]> {
        return this.getProgramTagsUrl(programId).then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                map(response => response as string[]))
                .toPromise());
    }

    getProgramLogs(programId: string): Promise<ProgramLogEntry[]> {
        return (this.getProgramLogsUrl(programId)
                .then(url =>
                      this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                      .toPromise()) as Promise<ProgramLogEntry[]>);
    }

    public async createProgram(programType?: ProgramType, programName?: string): Promise<ProgramMetadata> {
        const data: { type?: string, name?: string } = {};
        if (programType) {
            data.type = programType;
        }
        if (programName) {
            data.name = programName;
        }

        const url = await this.getCreateProgramsUrl();
        return await this.http
            .post(url, JSON.stringify(data), {
                headers: this.sessionService.addJsonContentType(this.sessionService.getAuthHeader())
            }).toPromise() as Promise<ProgramMetadata>;
    }

    public async createProgramOnGroup(programType: ProgramType, programName: string | null, groupId: string): Promise<ProgramMetadata> {
        const data: { type?: string, name?: string } = {};
        if (programType) {
            data.type = programType;
        }
        if (programName) {
            data.name = programName;
        }

        const url = this.getGroupCreateProgramsUrl(groupId);
        return await this.http
            .post(url, JSON.stringify(data), {
                headers: this.sessionService.addJsonContentType(this.sessionService.getAuthHeader())
            }).toPromise() as Promise<ProgramMetadata>;
    }

    updateProgram(program: ProgramContent): Promise<boolean> {
        return this.getUpdateProgramUrl(program.id).then(url =>
            this.http
                .put(url, JSON.stringify({type: program.type, orig: program.orig, parsed: program.parsed}),
                     {headers: this.sessionService.addContentType(
                                  this.sessionService.getAuthHeader(),
                         ContentType.Json)})
                .pipe(map(_ => true))
                .toPromise()
                .catch(_ => false)
        );
    }

    async updateProgramById(program: { id: string, type: ProgramType, orig: any, parsed: any, pages?: {[key: string]: any} }): Promise<boolean> {
        const url = await this.getUpdateProgramUrl(program.id);

        const data = {type: program.type, orig: program.orig, parsed: program.parsed, pages: program.pages};

        try {
            (await
             this.http
                 .put(url,
                      JSON.stringify(data),
                      {headers: this.sessionService.addContentType(
                          this.sessionService.getAuthHeader(),
                          ContentType.Json)})
                 .toPromise());
            return true;
        }
        catch (err) {
            console.error("Error updating program:", err);
            return false;
        }
    }

    updateProgramTags(programId: string, programTags: string[]): Promise<boolean> {
        return this.getProgramTagsUrl(programId).then(url =>
            this.http
                .post(url, JSON.stringify({tags: programTags}),
                     {headers: this.sessionService.addContentType(
                                  this.sessionService.getAuthHeader(),
                         ContentType.Json)})
                .pipe(map(_ => true))
                .toPromise()
                .catch(_ => false)
        );
    }

    renameProgram(program: ProgramContent, newName: string): Promise<boolean> {
        return this.getUpdateProgramUrl(program.id).then(
            url => (this.http
                    .patch(url,
                           JSON.stringify({name: newName}),
                           {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                        ContentType.Json)})
                .pipe(map(_ => true))
                .toPromise()));
    }

    async renameProgramById(programId: string, newName: string): Promise<boolean> {
        const url = await this.getUpdateProgramUrl(programId);
        const _response = await (this.http
                                .patch(url,
                                       JSON.stringify({name: newName}),
                                       {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                                    ContentType.Json)})
                                .toPromise());
        return true;
    }

    stopThreadsProgram(programId: string): Promise<boolean> {
        return this.getProgramStopThreadsUrl(programId).then(
            url => (this.http
                    .post(url,"",
                           {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                      ContentType.Json)}).pipe(
                    map(response => {
                        return true;
                    }))
                    .toPromise()));
    }

    setProgramStatus(status:string, programId:string){
        return this.getProgramsStatusUrl(programId).then(
            url => (this.http
                    .post(url,
                           status,
                           {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                        ContentType.Json)})
                .pipe(map(_ => true))
                .toPromise()));
    }

    deleteProgram(program: ProgramContent): Promise<boolean> {
        return this.getUpdateProgramUrl(program.id).then(
            url => (this.http
                    .delete(url,
                            {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                         ContentType.Json)})
                .pipe(map(_ => true))
                .toPromise()));
    }

    async deleteProgramById(programId: string): Promise<boolean> {
        const url = await this.getUpdateProgramUrl(programId);
        const _response = await(this.http
            .delete(url,
                    {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                 ContentType.Json)})
            .toPromise());
        return true;
    }

    async checkpointProgram(programId: string, content: any): Promise<void> {
        const url = await this.getProgramCheckpointUrlById(programId);
        const _response = await(
            this.http
                .post(url,
                      JSON.stringify(content),
                      {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                   ContentType.Json)})
                .toPromise());
    }

    public getPageUrl(programId: string, pagePath: string): any {
        let baseServerPath = document.location.origin;
        const apiHost = this.environmentService.getBrowserApiHost();
        if (apiHost != '') {
            baseServerPath = apiHost;
        }

        return `${baseServerPath}/api/v0/programs/by-id/${programId}/render${pagePath}`;
    }


    watchProgramLogs(programId: string, options: { request_previous_logs?: boolean }): Observable<ProgramInfoUpdate> {
        let websocket: WebSocket | null = null;

        return new Observable((observer) => {

            this.getProgramStreamingLogsUrl(programId).then(streamingUrl => {

                let buffer = [];
                let state : 'none_ready' | 'ws_ready' | 'all_ready' = 'none_ready';

                websocket = new WebSocket(streamingUrl);
                websocket.onopen = (() => {
                    if (options.request_previous_logs) {
                        state = 'ws_ready';

                        this.getProgramLogs(programId).then(entries => {
                            for (const entry of entries) {
                                observer.next({
                                    type: 'program_log',
                                    value: entry,
                                });
                            }

                            for (const entry of buffer) {
                                observer.next(entry);
                            }

                            buffer = []; // Empty buffer
                            state = 'all_ready';
                        });
                    }
                    else {
                        state = 'all_ready';
                    }
                });

                websocket.onmessage = ((ev) => {
                    if (state === 'ws_ready') {
                        buffer.push(JSON.parse(ev.data));
                    }
                    else {
                        observer.next(JSON.parse(ev.data));
                    }
                });

                websocket.onclose = (() => {
                    observer.complete();
                });

                websocket.onerror = ((ev) => {
                    observer.error(ev);
                    observer.complete();
                });
            });

            return () => {
                if (websocket) {
                    websocket.close();
                }
            }
        });
    }

    getEventStream(programId: string): Synchronizer<ProgramEditorEventValue> {
        let websocket: WebSocket | null = null;
        let sendBuffer = [];
        let state : 'none_ready' | 'ws_ready' | 'all_ready' | 'closed' = 'none_ready';

        const obs = new Observable<ProgramEditorEventValue>((observer) => {
            this.getProgramStreamingEventsUrl(programId).then(streamingUrl => {
                if (state === 'closed') {
                    return; // Cancel the opening of websocket if the stream was closed before being established
                }

                websocket = new WebSocket(streamingUrl)

                websocket.onopen = (() => {
                    state = 'all_ready';
                    for (const ev of sendBuffer) {
                        websocket.send(JSON.stringify(ev));
                    }
                    sendBuffer = null;
                });

                websocket.onmessage = ((ev) => {
                    const parsed: ProgramEditorEvent = JSON.parse(ev.data);
                    observer.next(parsed.value);
                });

                websocket.onclose = (() => {
                    observer.complete();
                });

                websocket.onerror = ((ev) => {
                    observer.error(ev);
                    observer.complete();
                });
            });

            return () => {
                if (websocket) {
                    websocket.close();
                }
            }
        });

        const sharedObserver = obs.pipe(share());
        return {
            subscribe: sharedObserver.subscribe.bind(sharedObserver),
            push: (ev: ProgramEditorEventValue) => {
                const msg = {type: 'editor_event', value: ev};
                if (state === 'none_ready') {
                    sendBuffer.push(msg);
                }
                else {
                    websocket.send(JSON.stringify(msg));
                }
            }
        };
    }
}
