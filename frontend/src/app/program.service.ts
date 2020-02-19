
import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { ProgramMetadata, ProgramContent, ProgramType } from './program';
import * as API from './api-config';


import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';

interface ProgramLogEntry {
    program_id: string,
    thread_id: string | 'none',
    user_id: string | 'none',
    block_id: string | undefined,
    severity: 'error' | 'debug' | 'warning',
    event_data: any,
    event_message: string,
    event_time: number,
};

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

    async getListProgramsUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/programs/';
    }

    async getCreateProgramsUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/programs/';
    }

    async getRetrieveProgramUrl(_user_id: string, program_id: string) {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/programs/' + program_id;
    }

    async getUpdateProgramUrl(programUserName: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUser(programUserName);
        return userApiRoot + '/programs/' + encodeURIComponent(program_id);
    }

    async getProgramTagsUrl(programUserId: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/tags';
    }

    private async getProgramLogsUrl(programUserId: string, program_id: string) {
        const userApiRoot = await this.sessionService.getApiRootForUserId(programUserId);
        return userApiRoot + '/programs/id/' + encodeURIComponent(program_id) + '/logs';
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

    getProgram(user_id: string, program_id: string): Promise<ProgramContent> {
        return this.getRetrieveProgramUrl(user_id, program_id).then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                map(response => response as ProgramContent))
                .toPromise());
    }

    getProgramTags(user_id: string, program_id: string): Promise<string[]> {
        return this.getProgramTagsUrl(user_id, program_id).then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                map(response => response as string[]))
                .toPromise());
    }

    getProgramLogs(user_id: string, program_id: string): Promise<string[]> {
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
}
