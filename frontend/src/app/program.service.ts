import { Injectable } from '@angular/core';
import { ProgramMetadata, ProgramContent, ProgramType } from './program';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';

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
      return userApiRoot + '/programs/' + program_id;
    }

    getPrograms(): Promise<ProgramMetadata[]> {
        return this.getListProgramsUrl().then(url =>
          this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                  .map(response => response as ProgramMetadata[])
                  .toPromise());
    }

    getProgram(user_id: string, program_id: string): Promise<ProgramContent> {
        return this.getRetrieveProgramUrl(user_id, program_id).then(url =>
            this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                .map(response => response as ProgramContent)
                .toPromise());
    }

    createProgram(): Promise<ProgramMetadata> {
        return this.getCreateProgramsUrl().then(url =>
            this.http
                .post(url, JSON.stringify({}),
                      {headers: this.sessionService.addJsonContentType(
                          this.sessionService.getAuthHeader())})
                .map(response => {
                    return response as ProgramMetadata;
                })
                .toPromise());
    }

    updateProgram(username: string,
                  program: ProgramContent): Promise<boolean> {
        return this.getUpdateProgramUrl(username, program.name).then(url =>
            this.http
                .put(url, JSON.stringify({type: program.type, orig: program.orig, parsed: program.parsed}),
                     {headers: this.sessionService.addContentType(
                                  this.sessionService.getAuthHeader(),
                                     ContentType.Json)})
                .map(response => {
                    return true;
                })
                .toPromise()
                .catch(_ => false)
        );
    }
}
