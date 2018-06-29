import { Injectable } from '@angular/core';
import { Service, AvailableService, ServiceEnableHowTo } from './service';
import * as API from './api-config';
import 'rxjs/add/operator/toPromise';
import 'rxjs/add/operator/map';
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

    async getServiceEnableHowToUrl(service: AvailableService) {
        return API.ApiHost + service.link + '/how-to-enable';
    }

    getAvailableServices(): Promise<AvailableService[]> {
        return this.getListAvailableServicesUrl().then(
            url => this.http.get(url, { headers: this.sessionService.getAuthHeader()})
                            .map(response => response as AvailableService[])
                            .toPromise());
    }

    getHowToEnable(service: AvailableService): Promise<ServiceEnableHowTo> {
        return this.getServiceEnableHowToUrl(service).then(
            url => this.http.get(url, {headers: this.sessionService.getAuthHeader()})
                            .map(response => response as ServiceEnableHowTo)
                            .toPromise());
    }
}
