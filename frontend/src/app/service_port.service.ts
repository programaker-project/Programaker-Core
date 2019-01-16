import { Injectable } from '@angular/core';
import { ServicePortMetadata } from './service_port';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
import { HttpClient } from '@angular/common/http';
import { SessionService } from './session.service';
import { ContentType } from './content-type';

@Injectable()
export class ServicePortService {
    constructor(
        private http: HttpClient,
        private sessionService: SessionService
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getCreateServicePortUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/service-port/';
    }

    createServicePort(name: string): Promise<ServicePortMetadata> {
        return this.getCreateServicePortUrl().then(url =>
            this.http.post(url, JSON.stringify({ name: name }),
                {
                    headers: this.sessionService.addJsonContentType(
                        this.sessionService.getAuthHeader())
                })
                .map(response => {
                    return response as ServicePortMetadata;
                })
                .toPromise());
    }

}
