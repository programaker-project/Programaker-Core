
import {map} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Service, AvailableService, ServiceEnableHowTo } from './service';
import * as API from './api-config';


import { SessionService } from './session.service';
import { HttpClient } from '@angular/common/http';
import { ContentType } from './content-type';

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

    async getServiceRegistryUrl(service_id: string) {
        const serviceRoot = await this.getListAvailableServicesUrl();
        return serviceRoot + 'id/' + service_id + '/register';

    }

    getAvailableServices(): Promise<AvailableService[]> {
        return this.getListAvailableServicesUrl().then(
            url => this.http.get(url, { headers: this.sessionService.getAuthHeader() }).pipe(
                map(response => response as AvailableService[]))
                .toPromise());
    }

    getHowToEnable(service: AvailableService): Promise<ServiceEnableHowTo> {
        return this.getServiceEnableHowToUrl(service).then(
            url => this.http.get(url, { headers: this.sessionService.getAuthHeader() }).pipe(
                map(response => {
                    return response as ServiceEnableHowTo;
                }))
                .toPromise());
    }

    registerService(service_id: string, data: { [key: string]: string }): Promise<boolean> {
        return this.getServiceRegistryUrl(service_id).then(
            url => this.http.post(
                url, JSON.stringify(data),
                {
                    headers: this.sessionService.addContentType(
                        this.sessionService.getAuthHeader(),
                        ContentType.Json),
                }).pipe(
                map(response => {
                    return (response as any).success;
                }))
                .toPromise());
    }
}
