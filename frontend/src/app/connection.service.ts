import { Injectable } from '@angular/core';
import { AvailableService } from './service';
import { BridgeIndexData } from './bridges/bridge';

import { BridgeConnection } from './connection';

import { SessionService } from './session.service';
import { HttpClient } from '@angular/common/http';
import { toWebsocketUrl } from './utils';


@Injectable()
export class ConnectionService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
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

    async getWaitForConnectionUrl(connection_id: string): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();

        const url = root + '/connections/pending/' + connection_id + '/wait';
        return toWebsocketUrl(url);
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

    async waitForPendingConnectionEstablished(connection_id: string): Promise<boolean> {
        const url = await this.getWaitForConnectionUrl(connection_id);

        return new Promise((resolve, reject) => {
            const websocket = new WebSocket(url);
            let completed = false;

            websocket.onopen = (() => {});

            websocket.onmessage = ((ev) => {
                const data = JSON.parse(ev.data);
                if (data.type === 'connection_established') {
                    try {
                        websocket.close();

                    }
                    catch(ex) {
                        console.warn("Error closing pending connection wait websocket", ex);
                    }

                    completed = true;
                    resolve(data.success);
                }
            });

            websocket.onclose = (() => {
                if (!completed) {
                    reject("closed before completion");
                }
            });

            websocket.onerror = ((ev) => {
                if (!completed) {
                    reject(ev);
                }
            });
        })
    }

    async toAvailableService(data: BridgeIndexData): Promise<AvailableService> {
        // TODO: Note this is not totally correct.
        // But it works for the __link__ part, which is what we need now.
        const root = await this.sessionService.getUserApiRoot();

        return {
            id: data.id,
            enabled: data.is_connected,
            link: `${root}/services/id/${data.id}`,
            name: data.name,
        }
    }
}
