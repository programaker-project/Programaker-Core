import { Injectable } from '@angular/core';
import { BridgeMetadata, BridgeIndexData } from './bridge';
import * as API from '../api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from '../session.service';
import { ContentType } from '../content-type';

@Injectable()
export class BridgeService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getBridgeIndexUrl(): Promise<string> {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/bridges/';
    }

    async getSpecificBridgeUrl(user_id: string, bridge_id: string): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId(user_id);

        return root + '/bridges/id/' + bridge_id;
    }

    createServicePort(name: string): Promise<BridgeMetadata> {
        return this.getBridgeIndexUrl().then(url =>
            this.http.post(url, JSON.stringify({ name: name }),
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
                .map(response => {
                    return response as BridgeMetadata;
                })
                .toPromise());
    }

    listUserBridges(): Promise<BridgeIndexData[]> {
        return this.getBridgeIndexUrl().then(url =>
            this.http.get(url,
                {
                    headers: this.sessionService.getAuthHeader()
                })
                .map(response => {
                    return response as BridgeIndexData[];
                })
                .toPromise());
    }

    deleteBridge(user_id: string, bridge_id: string): any {
        return this.getSpecificBridgeUrl(user_id, bridge_id).then(url =>
            this.http.post(url, JSON.stringify({ name: name }),
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
                .map(response => {
                    return response as BridgeMetadata;
                })
                .toPromise());
    }

}
