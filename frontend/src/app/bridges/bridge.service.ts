import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { addTokenQueryString, toWebsocketUrl } from 'app/utils';
import { Observable, Observer } from 'rxjs';
import * as API from '../api-config';
import { SessionService } from '../session.service';
import { BridgeIndexData, BridgeMetadata, BridgeResource, BridgeResourceEntry, BridgeResourceMap, BridgeSignal, FullOwnerId } from './bridge';



export type BridgeInfoUpdate = { count: number };

@Injectable()
export class BridgeService {
    // These static values help create a Singleton-like class.
    // While not strtictly following the Singleton pattern, as class instances
    //   are obtained through Dependency Injection, it allows for every instance
    //   of the class to share the relevant state.
    // The shared state is the established session, it's Observable and Observer.
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

    private async getBridgeIndexUrl(): Promise<string> {
        const userId = (await this.sessionService.getSession()).user_id;
        return `${API.ApiRoot}/users/id/${userId}/bridges`;
    }


    private getGroupBridgeIndexUrl(groupId: string): string {
        return `${API.ApiRoot}/groups/by-id/${groupId}/bridges`;
    }

    private getSpecificBridgeUrl(bridgeId: string): string {
        return `${API.ApiRoot}/bridges/by-id/${bridgeId}`;
    }

    private getBridgeSignalsUrl(bridgeId: string): string {
        const url = `${API.ApiRoot}/bridges/by-id/${bridgeId}/signals`;
        return toWebsocketUrl(url);
    }

    private getBridgeResourcesUrl(bridgeId: string): string {
        return `${API.ApiRoot}/bridges/by-id/${bridgeId}/resources`;
    }

    private updateConnectionResourceUrl(connectionId: string, resourceName: string): string {
        return `${API.ApiRoot}/connections/by-id/${connectionId}/resources/by-name/${resourceName}`;
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

    async createGroupBridge(name: string, groupId: string): Promise<BridgeMetadata> {
        const url = this.getGroupBridgeIndexUrl(groupId);

        const response = (await this.http.post(url, JSON.stringify({ name: name }),
                                               { headers: this.sessionService.addJsonContentType(
                                                   this.sessionService.getAuthHeader())
                                               }).toPromise());

        return response as BridgeMetadata;
    }

    async listGroupBridges(groupId: string): Promise<BridgeIndexData[]> {
        const url = this.getGroupBridgeIndexUrl(groupId);
        const response = (await this.http.get(url,
                                              { headers: this.sessionService.getAuthHeader()})
                          .toPromise());

        return response as BridgeIndexData[];
    }

    async deleteBridge(bridgeId: string): Promise<boolean> {
        const url = await this.getSpecificBridgeUrl(bridgeId);
        const response = (await this.http.delete(url,
                                                 { headers: this.sessionService.getAuthHeader() })
                          .toPromise());

        // We would have to know who is the owner to do this appropriately
        // @TODO
        // if (BridgeService.bridgeCount !== null) {
        //     BridgeService.bridgeCount -= 1;
        //     BridgeService._bridgeInfoObserver.next({
        //         count: BridgeService.bridgeCount
        //     });
        // }

        return (response as { success: boolean }).success;
    }

    async setShares(connectionId: string, resourceName: string, shares: {[key:string]: FullOwnerId[]}, options?: { asGroup?: string; }) {
        let url = this.updateConnectionResourceUrl(connectionId, resourceName);
        if (options && options.asGroup) {
            url += '&as_group=' + options.asGroup;
        }

        const response = (await this.http.patch(url, { shared: shares },
                                                { headers: this.sessionService.addJsonContentType(
                                                    this.sessionService.getAuthHeader())
                                                }).toPromise());
    }

    async getBridgeResources(bridgeId: string, asGroup?: string): Promise<BridgeResource[]> {
        let url = this.getBridgeResourcesUrl(bridgeId);
        if (asGroup) {
            url += '&as_group=' + asGroup;
        }

        const response = (await this.http.get(url,
                                              { headers: this.sessionService.getAuthHeader() })
            .toPromise()) as BridgeResourceMap;

        const resources: BridgeResource[] = [];

        for (const resource of Object.keys(response)) {
            const values: BridgeResourceEntry[] = [];

            for (const valId of Object.keys(response[resource])) {
                const value = response[resource][valId];
                values.push({ id: valId, name: value.name, connection_id: value.connection_id })
            }

            resources.push({ name: resource, values: values });
        }

        return resources;
    }

    getBridgeSignals(bridgeId: string, asGroup?: string): Observable<BridgeSignal> {
        const token = this.sessionService.getToken();

        let url = addTokenQueryString(this.getBridgeSignalsUrl(bridgeId), token);
        if (asGroup) {
            url += '&as_group=' + asGroup;
        }

        let manuallyClosed = false;

        return new Observable<BridgeSignal>((observer) => {
            const websocket = new WebSocket(url);
            websocket.onopen = (() => {
                console.log("Connected");
            });

            websocket.onmessage = ((ev) => {
                if (typeof(ev.data) === "string" ){
                    observer.next(JSON.parse(ev.data));
                }
                else {
                    const reader = new FileReader();

                    // This fires after the blob has been read/loaded.
                    reader.addEventListener('loadend', (e) => {
                        const text = (e.srcElement as any).result;
                        observer.next(JSON.parse(text));
                    });

                    reader.readAsText(ev.data);
                }
            });

            websocket.onclose = (() => {
                if (!manuallyClosed) {
                    console.error("Complete");
                }
                observer.complete();
            });

            websocket.onerror = ((ev) => {
                if (!manuallyClosed) {
                    console.error("Error");
                }
                observer.error(ev);
            });

            // Unsubscription procedure
            return () => {
                manuallyClosed = true;
                websocket.close();
            };
        });
    }
}
