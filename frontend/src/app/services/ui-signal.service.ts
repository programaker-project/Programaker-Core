import { Injectable, EventEmitter } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { SessionService } from 'app/session.service';
import { EnvironmentService } from 'app/environment.service';
import { toWebsocketUrl } from 'app/utils';
import { Observable } from 'rxjs';
import { startWith } from 'rxjs/operators';

type Message = any;

@Injectable()
export class UiSignalService {
    programId: string;
    websocketEstablishment: Promise<WebSocket>;
    messageEmitter: EventEmitter<any>;
    private _gotInitialValues: boolean | 'in-progress' = false;
    private _lastMessages: {[key: string]: any} = {};

    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
        private environmentService: EnvironmentService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
        this.messageEmitter = new EventEmitter<Message>(false);
    }

    setProgramId(programId: string) {
        this.programId = programId;
    }

    private _assertInitialized() {
        if (!this.programId) {
            throw Error("UiSignalService non initialized, ProgramId not set.");
        }
    }


    _getInitialValuesUrl(): string {
        return `${this.environmentService.getApiRoot()}/programs/by-id/${this.programId}/ui-values`;
    }

    private _pullInitialValues() {
        if (this._gotInitialValues) {
            return;
        }
        this._gotInitialValues = 'in-progress';

        const url = this._getInitialValuesUrl();
        (this.http.get(url, {headers: this.sessionService.getAuthHeader()})
            .toPromise()
            .then((resp: any) => {

                const vals = resp.widget_values;
                for (const widgetId of Object.keys(vals)) {
                    if (!this._lastMessages[widgetId]) {

                        const message = {
                            key: 'ui_events_show',
                            subkey: widgetId,
                            values: vals[widgetId],
                        };
                        this._lastMessages[widgetId] = message;
                        this.messageEmitter.emit(message);
                    }
                }

                this._gotInitialValues = true;
            }));
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
                    const parsed = JSON.parse(ev.data);
                    this.messageEmitter.emit(parsed);
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

    public onElementUpdate(blockType: string, blockId: string): Observable<any> {
        this._assertInitialized();
        this._getWebsocket(); // Create websocket if not existing

        if (!this._gotInitialValues) {
            this._pullInitialValues();
        }

        const selector = `${blockType}.${blockId}`;

        const observer = new Observable(observer => {
            this.messageEmitter.subscribe({
                next: (ev) => {
                    if (ev.subkey === selector) {
                        this._lastMessages[selector] = ev;
                        observer.next(ev);
                    }
                }});
        })

        if (this._lastMessages[selector]) {
            return observer.pipe(startWith(this._lastMessages[selector]));
        }
        else {
            return observer;
        }

    }
}
