import { map, share} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { ProgramMetadata, ProgramContent, ProgramInfoUpdate, ProgramLogEntry, ProgramType, ProgramEditorEvent, ProgramEditorEventValue } from './program';

import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';
import { toWebsocketUrl } from './utils';
import { ApiRoot } from './api-config';
import { Synchronizer } from './syncronizer';

@Injectable()
export class ProgramService {
    private getExamplesForProgramRootUrl = '/api/programs/examples/';
    private addExampleToProgramRootUrl = '/api/programs/examples/';

    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    private addTokenQueryString(url: string, token: string): string {
        if (url.indexOf('?') === -1) {
            return url + '?token=' + token;
        }
        else {
            return url + '&token=' + token;
        }
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
        return `${ApiRoot}/groups/by-id/${groupId}/programs`;
    }

    private listProgramsOnGroupUrl(groupId: string) {
        return `${ApiRoot}/groups/by-id/${groupId}/programs`;
    }

    private getRetrieveProgramUrl(user_id: string, program_name: string) {
        return `${ApiRoot}/users/${user_id}/programs/${program_name}`;
    }

    private getRetrieveProgramUrlById(program_id: string): string {
        return ApiRoot + '/programs/id/' + program_id;
    }

    async getUpdateProgramUrl(program_id: string) {
        return ApiRoot + '/programs/id/' + program_id;
    }

    async getProgramCheckpointUrlById(program_id: string, user_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(user_id);
        return `${userApiRoot}/programs/id/${program_id}/checkpoint`;
    }

    async getProgramTagsUrl(programUserId: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/tags';
    }

    private async getProgramLogsUrl(programUserId: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/logs';
    }

    private async getProgramStreamingLogsUrl(programUserId: string, program_id: string) {
        const token = this.sessionService.getToken();
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return this.addTokenQueryString(toWebsocketUrl(userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/logs-stream'),
                                  token,
                                 );
    }

    private async getProgramStreamingEventsUrl(programUserId: string, program_id: string) {
        const token = this.sessionService.getToken();
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return this.addTokenQueryString(toWebsocketUrl(userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/editor-events'),
                                        token,
                                       );
    }

    async getProgramStopThreadsUrl(programUserId: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/stop-threads';
    }

    async getProgramsStatusUrl(programUserId: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/status';
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

    async getProgram(user_name: string, program_name: string): Promise<ProgramContent> {
        const url = this.getRetrieveProgramUrl(user_name, program_name)

        const result = await this.http.get(url, {headers: this.sessionService.getAuthHeader()}).toPromise();

        return result as ProgramContent;
    }

    async getProgramById(program_id: string): Promise<ProgramContent> {
        const url = await this.getRetrieveProgramUrlById(program_id);
        return (this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                .toPromise() as Promise<ProgramContent>);
    }

    getProgramTags(user_id: string, program_id: string): Promise<string[]> {
        return this.getProgramTagsUrl(user_id, program_id).then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                map(response => response as string[]))
                .toPromise());
    }

    getProgramLogs(user_id: string, program_id: string): Promise<ProgramLogEntry[]> {
        return (this.getProgramLogsUrl(user_id, program_id)
                .then(url =>
                      this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                      .toPromise()) as Promise<ProgramLogEntry[]>);
    }

    public async createProgram(program_type?: ProgramType, program_name?: string): Promise<ProgramMetadata> {
        const data: { type?: string, name?: string } = {};
        if (program_type) {
            data.type = program_type;
        }
        if (program_name) {
            data.name = program_name;
        }

        const url = await this.getCreateProgramsUrl();
        return await this.http
            .post(url, JSON.stringify(data), {
                headers: this.sessionService.addJsonContentType(this.sessionService.getAuthHeader())
            }).toPromise() as Promise<ProgramMetadata>;
    }

    public async createProgramOnGroup(program_type: ProgramType, program_name: string | null, groupId: string): Promise<ProgramMetadata> {
        const data: { type?: string, name?: string } = {};
        if (program_type) {
            data.type = program_type;
        }
        if (program_name) {
            data.name = program_name;
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
                                     ContentType.Json)}).pipe(
                map(response => {
                    return true;
                }))
                .toPromise()
                .catch(_ => false)
        );
    }

    async updateProgramById(program: { id: string, type: ProgramType, orig: any, parsed: any }): Promise<boolean> {
        const url = await this.getUpdateProgramUrl(program.id);

        try {
            (await
             this.http
                 .put(url,
                      JSON.stringify({type: program.type, orig: program.orig, parsed: program.parsed}),
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

    updateProgramTags(user_id: string, program_id: string, programTags: string[]): Promise<boolean> {
        return this.getProgramTagsUrl(user_id, program_id).then(url =>
            this.http
                .post(url, JSON.stringify({tags: programTags}),
                     {headers: this.sessionService.addContentType(
                                  this.sessionService.getAuthHeader(),
                                     ContentType.Json)}).pipe(
                map(response => {
                    return true;
                }))
                .toPromise()
                .catch(_ => false)
        );
    }

    renameProgram(program: ProgramContent, new_name: string): Promise<boolean> {
        return this.getUpdateProgramUrl(program.id).then(
            url => (this.http
                    .patch(url,
                           JSON.stringify({name: new_name}),
                           {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                      ContentType.Json)}).pipe(
                    map(response => {
                        console.log("R:", response);
                        return true;
                    }))
                    .toPromise()));
    }

    async renameProgramById(program_id: string, new_name: string): Promise<boolean> {
        const url = await this.getUpdateProgramUrl(program_id);
        const _response = await (this.http
                                .patch(url,
                                       JSON.stringify({name: new_name}),
                                       {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                                    ContentType.Json)})
                                .toPromise());
        return true;
    }

    stopThreadsProgram(user_id: string, program_id: string): Promise<boolean> {
        return this.getProgramStopThreadsUrl(user_id, program_id).then(
            url => (this.http
                    .post(url,"",
                           {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                      ContentType.Json)}).pipe(
                    map(response => {
                        console.log("R:", response);
                        return true;
                    }))
                    .toPromise()));
    }

    setProgramStatus(status:string, program_id:string, user_id:string){
        return this.getProgramsStatusUrl(user_id, program_id).then(
            url => (this.http
                    .post(url,
                           status,
                           {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                      ContentType.Json)}).pipe(
                    map(response => {
                        console.log("R:", response);
                        return true;
                    }))
                    .toPromise()));
    }

    deleteProgram(program: ProgramContent): Promise<boolean> {
        return this.getUpdateProgramUrl(program.id).then(
            url => (this.http
                    .delete(url,
                            {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                         ContentType.Json)}).pipe(
                    map(response => {
                        console.log("R:", response);
                        return true;
                    }))
                    .toPromise()));
    }

    async deleteProgramById(program_id: string): Promise<boolean> {
        const url = await this.getUpdateProgramUrl(program_id);
        const _response = await(this.http
            .delete(url,
                    {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                 ContentType.Json)})
            .toPromise());
        return true;
    }

    async checkpointProgram(program_id: string, user_id: string, content: any): Promise<void> {
        const url = await this.getProgramCheckpointUrlById(program_id, user_id);
        const _response = await(
            this.http
                .post(url,
                      JSON.stringify(content),
                      {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                   ContentType.Json)})
                .toPromise());
    }

    watchProgramLogs(user_id: string, program_id: string, options: { request_previous_logs?: boolean }): Observable<ProgramInfoUpdate> {
        return new Observable((observer) => {

            this.getProgramStreamingLogsUrl(user_id, program_id).then(streamingUrl => {

                let buffer = [];
                let state : 'none_ready' | 'ws_ready' | 'all_ready' = 'none_ready';

                const websocket = new WebSocket(streamingUrl);
                websocket.onopen = (() => {
                    if (options.request_previous_logs) {
                        state = 'ws_ready';

                        this.getProgramLogs(user_id, program_id).then(entries => {
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
        });
    }

    getEventStream(user_id: string, program_id: string): Synchronizer<ProgramEditorEventValue> {
        let websocket: WebSocket | null = null;
        let sendBuffer = [];
        let state : 'none_ready' | 'ws_ready' | 'all_ready' | 'closed' = 'none_ready';

        const obs = new Observable<ProgramEditorEventValue>((observer) => {
            this.getProgramStreamingEventsUrl(user_id, program_id).then(streamingUrl => {
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
        });

        const sharedObserver = obs.pipe(share());
        return {
            subscribe: sharedObserver.subscribe.bind(sharedObserver),
            close: () => {
                state = 'closed';
                if (websocket) {
                    websocket.close();
                }
            },
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
