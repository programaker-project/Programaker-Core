import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BridgeIndexData } from './bridges/bridge';
import { BridgeConnection } from './connection';
import { AvailableService } from './service';
import { SessionService } from './session.service';
import { toWebsocketUrl } from './utils';
import { EnvironmentService } from './environment.service';


@Injectable()
export class ConnectionService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) { }

    async getAvailableConnectionsUrl(): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();

        return root + '/connections/available/';
    }

    getGroupAvailableConnectionsUrl(groupId: string): string {
        return `${this.environmentService.getApiRoot()}/groups/by-id/${groupId}/connections/available`;
    }

    async getUserConnectionsUrl(): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();

        return root + '/connections/established';
    }

    getGroupConnectionsUrl(groupId: string): string {
        return `${this.environmentService.getApiRoot()}/groups/by-id/${groupId}/connections/established`;
    }

    getProgramConnectionsUrl(programId: string): string {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/connections/established`;
    }

    getProgramAvailableConnectionsUrl(programId: string): string {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${programId}/connections/available`;
    }

    private getConnectionUrl(connectionId: string): string {
        return `${this.environmentService.getApiRoot()}/connections/by-id/${connectionId}`;
    }

    async getWaitForConnectionUrl(connection_id: string): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();

        const url = root + '/connections/pending/' + connection_id + '/wait';
        return toWebsocketUrl(this.environmentService, url);
    }

    async getAvailableBridgesForNewConnection(): Promise<BridgeIndexData[]> {
        const url = await this.getAvailableConnectionsUrl()
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<BridgeIndexData[]>);
    }

    public getAvailableBridgesForNewConnectionOnGroup(groupId: string): Promise<BridgeIndexData[]> {
        const url = this.getGroupAvailableConnectionsUrl(groupId);
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<BridgeIndexData[]>);
    }

    public getAvailableBridgesForNewConnectionOnProgram(programId: string): Promise<BridgeIndexData[]> {
        const url = this.getProgramAvailableConnectionsUrl(programId);
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

    public getConnectionsOnGroup(groupId: string): Promise<BridgeConnection[]> {
        const url = this.getGroupConnectionsUrl(groupId)
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<BridgeConnection[]>);
    }

    public getConnectionsOnProgram(programId: string): Promise<BridgeConnection[]> {
        const url = this.getProgramConnectionsUrl(programId)
        return (this.http.get(url,
                              { headers: this.sessionService.getAuthHeader() }
                             ).toPromise() as Promise<BridgeConnection[]>);
    }

    public async setRecordConnectionsSignal(connectionId: string, saveSignals: boolean, asGroup?: string): Promise<void> {
        let url = this.getConnectionUrl(connectionId);
        if (asGroup) {
            url += '?as_group=' + asGroup;
        }

        const _response = (await this.http.patch(url, { save_signals: saveSignals },
                                                 { headers: this.sessionService.addJsonContentType(
                                                     this.sessionService.getAuthHeader())
                                                 }).toPromise());
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
