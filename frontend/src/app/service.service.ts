import { Injectable } from '@angular/core';
import { Service, AvailableService } from './service';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import { SessionService } from './session.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class ServiceService {
    private getServicesUrl = API.ApiRoot + '/services/';

    constructor(
        private http: HttpClient,
        private sessionService: SessionService,
    ) {
        this.http = http;
        this.sessionService = sessionService;
    }

    async getListAvailableServicesUrl() {
        const userApiRoot = await this.sessionService.getUserApiRoot();
        return userApiRoot + '/services/';
    }

    getAvailableServices(): Promise<AvailableService[]> {
        return this.getListAvailableServicesUrl().then(
            url => this.http.get(url, { headers: this.sessionService.getAuthHeader()})
                            .map(response => response as AvailableService[])
                            .toPromise());
    }
}
