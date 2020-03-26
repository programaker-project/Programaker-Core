
import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { ProgramMetadata, ProgramContent, ProgramInfoUpdate, ProgramLogEntry } from './program';

import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';
import { toWebsocketUrl } from './utils';
import { ApiRoot } from './api-config';

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

    async getRetrieveProgramUrl(_user_id: string, program_name: string) {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/programs/' + program_name;
    }

    private getRetrieveProgramUrlById(program_id: string): string {
        return ApiRoot + '/programs/id/' + program_id;
    }

    async getUpdateProgramUrl(programUserName: string, program_id: string) {
        const userApiRoot = this.sessionService.getApiRootForUser(programUserName);
        return userApiRoot + '/programs/' + encodeURIComponent(program_id);
    }

    async getUpdateProgramUrlById(program_id: string) {
        return ApiRoot + '/programs/id/' + program_id;
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
        return this.addTokenQueryString(toWebsocketUrl(userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/communication'),
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

    getProgram(user_id: string, program_name: string): Promise<ProgramContent> {
        return this.getRetrieveProgramUrl(user_id, program_name).then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                map(response => response as ProgramContent))
                .toPromise());
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

    createProgram(): Promise<ProgramMetadata> {
        return this.getCreateProgramsUrl().then(url =>
            this.http
                .post(url, JSON.stringify({}),
                      {headers: this.sessionService.addJsonContentType(
                          this.sessionService.getAuthHeader())}).pipe(
                map(response => {
                    return response as ProgramMetadata;
                }))
                .toPromise());
    }

    updateProgram(username: string,
                  program: ProgramContent): Promise<boolean> {
        return this.getUpdateProgramUrl(username, program.name).then(url =>
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

    renameProgram(username: string, program: ProgramContent, new_name: string): Promise<boolean> {
        return this.getUpdateProgramUrl(username, program.name).then(
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
        const url = await this.getUpdateProgramUrlById(program_id);
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

    deleteProgram(username: string, program: ProgramContent): Promise<boolean> {
        return this.getUpdateProgramUrl(username, program.name).then(
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
        const url = await this.getUpdateProgramUrlById(program_id);
        const _response = await(this.http
                                .delete(url,
                                        {headers: this.sessionService.addContentType(this.sessionService.getAuthHeader(),
                                                                                     ContentType.Json)})
                                .toPromise());
        return true;
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

}
