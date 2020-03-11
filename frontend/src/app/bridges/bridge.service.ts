import { Observable, Observer } from 'rxjs';
import { Injectable } from '@angular/core';
import { BridgeMetadata, BridgeIndexData } from './bridge';
import * as API from '../api-config';


import { HttpClient } from '@angular/common/http';
import { SessionService } from '../session.service';
import { ContentType } from '../content-type';

export type BridgeInfoUpdate = { count: number };

@Injectable()
export class BridgeService {
    // These static values help create a Singleton-like class.
    // While not strtictly following the Singleton pattern, as class instances
    //   are obtained through Dependency Injection, it allows for every instance
    //   of the class to share the relevant state.
    // The shared state is theestablished session, it's Observable and Observer.
    static bridgeInfoObservable: Observable<BridgeInfoUpdate> = null;
    private static _bridgeInfoObserver: Observer<BridgeInfoUpdate> = null;
    private static bridgeCount: number = null;


    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
        BridgeService.bridgeInfoObservable = new Observable<BridgeInfoUpdate>((observer) => {
            BridgeService._bridgeInfoObserver = observer;
        });
    }

    async getBridgeIndexUrl(): Promise<string> {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/bridges/';
    }

    async getSpecificBridgeUrl(user_id: string, bridge_id: string): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId(user_id);

        return root + '/bridges/id/' + bridge_id;
    }

    async createServicePort(name: string): Promise<BridgeMetadata> {
        const url = await this.getBridgeIndexUrl();


        const response = (await this.http.post(url, JSON.stringify({ name: name }),
                                               { headers: this.sessionService.addJsonContentType(
                                                   this.sessionService.getAuthHeader())
                                               }).toPromise());

        if (BridgeService.bridgeCount !== null) {
            BridgeService.bridgeCount += 1;
            BridgeService._bridgeInfoObserver.next({ count: BridgeService.bridgeCount });
        }

        return response as BridgeMetadata;
    }

    async listUserBridges(): Promise<{bridges: BridgeIndexData[], monitor: Observable<BridgeInfoUpdate>}> {
        const url = await this.getBridgeIndexUrl();
        const response = (await this.http.get(url,
                                              { headers: this.sessionService.getAuthHeader()})
                          .toPromise());

        const bridgeData = response as BridgeIndexData[];
        BridgeService.bridgeCount = bridgeData.length;

        return {
            bridges: bridgeData,
            monitor: BridgeService.bridgeInfoObservable,
        };
    }

    async deleteBridge(user_id: string, bridge_id: string): Promise<boolean> {
        const url = await this.getSpecificBridgeUrl(user_id, bridge_id);
        const response = (await this.http.delete(url,
                                                 { headers: this.sessionService.getAuthHeader() })
                          .toPromise());

        if (BridgeService.bridgeCount !== null) {
            BridgeService.bridgeCount -= 1;
            BridgeService._bridgeInfoObserver.next({
                count: BridgeService.bridgeCount
            });
        }

        return (response as { success: boolean }).success;
    }
}
