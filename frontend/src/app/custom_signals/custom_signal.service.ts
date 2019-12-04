import { Injectable } from '@angular/core';
import { CustomSignal, CustomSignalCreationResponse } from './custom_signal';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from '../session.service';

@Injectable()
export class CustomSignalService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getCustomSignalIndexUrl(): Promise<string> {
        const root = await this.sessionService.getApiRootForUserId();
        return root + '/custom_signals/';
    }

    async saveCustomSignal(custom_signal_name: string): Promise<CustomSignalCreationResponse> {
        const indexUrl = await this.getCustomSignalIndexUrl();

        return this.http
            .post(indexUrl, JSON.stringify({
                name: custom_signal_name,
            }),
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
            .map(response => {
                return response as CustomSignalCreationResponse;
            })
            .toPromise();
    }

    async getCustomSignals(): Promise<CustomSignal[]> {
        const indexUrl = await this.getCustomSignalIndexUrl();

        return this.http
            .get(indexUrl, {
                headers: this.sessionService.getAuthHeader()
            })
            .map(response => {
                return response as CustomSignal[];
            })
            .toPromise();
    }

}
