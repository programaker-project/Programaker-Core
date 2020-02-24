import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Service, AvailableService, ServiceEnableHowTo } from './service';
import { BridgeService } from './bridges/bridge.service';
import { BridgeIndexData } from './bridges/bridge';
import * as API from './api-config';

import { BridgeConnection } from './connection';

import { SessionService } from './session.service';
import { ServiceService } from './service.service';
import { HttpClient } from '@angular/common/http';
import { ContentType } from './content-type';

/**
 * This is a Mock to experiment with connection-management flows.
 *
 */

@Injectable()
export class ConnectionService {
    constructor(
        private http: HttpClient,
        private bridgeService: BridgeService,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.bridgeService = bridgeService;
        this.sessionService = sessionService;
    }

    async getAvailableConnectionsUrl(): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();

        return root + '/connections/available/';
    }

    async getUserConnectionsUrl(): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();

        return root + '/connections/established';
    }

    async getAvailableBridgesForNewConnection(): Promise<BridgeIndexData[]> {
        const url = await this.getAvailableConnectionsUrl()
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<BridgeIndexData[]>);
    }

    async getConnections(): Promise<BridgeConnection[]> {
        const url = await this.getUserConnectionsUrl()
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<BridgeConnection[]>);
    }

    toAvailableService(data: BridgeIndexData): AvailableService {
        // TODO: Note this is not totally correct.
        // But it works for the __link__ part, which is what we need now.
        return {
            id: data.id,
            enabled: data.is_connetcted,
            link: `/api/v0/users/test/services/id/${data.id}`,
            name: data.name,
        }
    }
}
