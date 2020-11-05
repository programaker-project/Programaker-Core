import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { SessionService } from 'app/session.service';
import { EnvironmentService } from 'app/environment.service';
import { toWebsocketUrl } from 'app/utils';

@Injectable()
export class UiSignalService {
    programId: string;
    websocketEstablishment: Promise<WebSocket>;

    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    setProgramId(programId: string) {
        this.programId = programId;
    }

    private _assertInitialized() {
        if (!this.programId) {
            throw Error("UiSignalService non initialized, ProgramId not set.");
        }
    }

    _getWebsocketUrl(): string {
        return toWebsocketUrl(this.environmentService,
                              `${this.environmentService.getApiRoot()}/programs/by-id/${this.programId}/ui-events`);
    }

    private _clearWebsocket() {
        this.websocketEstablishment = null;
    }

    private _getWebsocket(): Promise<WebSocket> {
        if (!this.websocketEstablishment) {
            this.websocketEstablishment = new Promise((resolve, reject) => {
                const websocket = new WebSocket(this._getWebsocketUrl());

                websocket.onopen = (() => {
                    const token = this.sessionService.getToken();

                    // Authenticate
                    websocket.send(JSON.stringify({
                        "type": "AUTHENTICATION",
                        "value": {"token": token}
                    }));

                    resolve(websocket);
                });

                websocket.onmessage = ((ev) => {
                    console.debug("New message:", ev);
                });

                websocket.onclose = (() => {
                    this._onWebscoketClose();
                });

                websocket.onerror = ((ev) => {
                    this._onWebsocketError(ev);
                    reject(ev);
                });

            });

            this.websocketEstablishment.catch(err => {
                this._clearWebsocket();
            })
        }

        return this.websocketEstablishment;
    }

    _onWebsocketError(ev) {
        console.error("Websocket error", ev);
        this._onWebscoketClose();
    }

    _onWebscoketClose() {
        this._clearWebsocket();
    }


    public async sendBlockSignal(blockType: string, blockId: string): Promise<void> {
        this._assertInitialized();

        const ws = await this._getWebsocket();
        ws.send(JSON.stringify({
            type: "ui-event",
            value: {
                action: "activated",
                block_type: blockType,
                block_id: blockId,
            }
        }));
    }
}
