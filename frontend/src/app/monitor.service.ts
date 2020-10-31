import {map} from 'rxjs/operators';
import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';

import { MonitorMetadata } from './monitor';
import { SessionService } from './session.service';
import { EnvironmentService } from './environment.service';

@Injectable()
export class MonitorService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    public async getListMonitorsUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/monitors/';
    }

    public async getListMonitorsOnProgramUrl(programId: string) {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/monitors`;
    }

    public async getRetrieveMonitorUrl(_user_id: string, Monitor_id: string) {
      const userApiRoot = await this.sessionService.getUserApiRoot();
      return userApiRoot + '/monitors/' + Monitor_id;
    }

    public async getUpdateMonitorUrl(MonitorUserName: string, Monitor_id: string) {
      const userApiRoot = await this.sessionService.getApiRootForUser(MonitorUserName);
      return userApiRoot + '/monitors/' + Monitor_id;
    }

    public getMonitors(): Promise<MonitorMetadata[]> {
        return this.getListMonitorsUrl().then((url) =>
          this.http.get(url, {headers: this.sessionService.getAuthHeader()}).pipe(
                  map((response) => {
                      return response as MonitorMetadata[]
                  }))
                  .toPromise());
    }

    public async getMonitorsOnProgram(programId: string): Promise<MonitorMetadata[]> {
        const url = await this.getListMonitorsOnProgramUrl(programId);

        return this.http.get(url, { headers: this.sessionService.getAuthHeader() }).pipe(
            map((response) => {
                return response as MonitorMetadata[];
            }))
            .toPromise();
    }
}
